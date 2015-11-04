module Desugar where

import Data.Function (on)
import Data.Functor.Foldable
import qualified Data.HashMap as H
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Expr
import Literal
import Patt
import Sugar
import Token (Id)

-- | A floating expression fragment, waiting to be placed within a particular
-- scope.
type IxExpr = Int
           -- ^ The level at which this floating expression is being inserted.
           -> H.Map Id Int
           -- ^ A mapping from the identifiers of currently bound variables to
           -- the levels in the AST they were bound at.
           -> Expr

desugarExpr :: Sugar -> Expr
desugarExpr s = cata d s 0 H.empty
  where
    d :: Base Sugar IxExpr -> IxExpr
    d (VarSB x)         = var x
    d (AppSB x es)      = apply x es
    d (RangeSB f t)     = apply "_range" [f, t]
    d (LSectSB x e)     = apply "_lsect" [var x, e]
    d (RSectSB e x)     = apply "_rsect" [var x, e]
    d (IfSB c t e)      = embedIx $ IfEB c t e
    d (SeqSB a b)       = embedIx $ SeqEB a b
    d (LitSB l)         = embedIx $ LitEB l
    d (LetSB x a b)     = letIx x a b
    d (ListCompSB e gs) = compileListComp e gs (close nilB)
    d (FnSB arms)       = compileFn arms

-- | Convert an expression with floating children into a floating expression
-- itself.
embedIx :: Base Expr IxExpr -> IxExpr
embedIx e l vs = embed $ (\ix -> ix l vs) <$> e

-- | Close over an expression with no dangling bound variables.
close :: Expr -> IxExpr
close e _ _ = e

-- | Creating a floating expression that will reference a variable introduced at
-- level @ l' @ when reified.
local :: Int -> IxExpr
local l' l _ = VarE (l - l')

-- | Convert a variable in the source language to potentially a de Bruijn
-- index, if the variable with that identifier appears bound within the scope
-- this fragment is reified in.
var :: Id -> IxExpr
var v l vs =
  case H.lookup v vs of
    Just l' -> VarE (l - l')
    Nothing -> FreeE v

-- | Convert a function application in the source language into one in the
-- desugared language.
apply :: Id -> [IxExpr] -> IxExpr
apply x xs = embedIx (AppEB (var x) xs)

-- | Desugaring of (possibly recursive) let expressions. This is not a simple
-- embedding because a @ let @ expression introduces a variable.
letIx :: Id -> IxExpr -> IxExpr -> IxExpr
letIx x a b l vs = LetE (a l' vs') (b l' vs')
  where
    l'  = l + 1
    vs' = H.insert x l vs

compileListComp :: IxExpr
                -- ^ The element expression
                -> [GenB IxExpr]
                -- ^ The list of generators
                -> IxExpr
                -- ^ The tail of the result
                -> IxExpr

compileListComp eix []     aix = embedIx (LitEB (ConsB eix aix))
compileListComp eix (g:gs) aix = compileGen g aix (compileListComp eix gs)
  where
    compileGen :: GenB IxExpr
               -- ^ A generator
               -> IxExpr
               -- ^ The tail of the result
               -> (IxExpr -> IxExpr)
               -- ^ A continuation, to generate the inner loop of the generator
               -> IxExpr
    compileGen (FilterB pix) bix inner = embedIx (IfEB pix (inner bix) bix)
    compileGen (GenB p gix)  bix inner = \ l vs ->
      let cix = local (l+1)
          iix = inner cix
          fix = compileFn [ FnArm "" [p,          (VarP "_")] iix Nothing
                          , FnArm "" [(VarP "_"), (VarP "_")] cix Nothing
                          ]
      in apply "_mapa" [fix, gix, bix] l vs

compileFn :: [FnArmB IxExpr] -> IxExpr
compileFn []       _ _  = error "compileFn: No arms in function body."
compileFn as@(a:_) l vs = FnE arity body
  where
    arity = let FnArm _ ps _ _ = a in length ps
    xs    = map local [l .. l + arity - 1]
    body  = compileCase xs (close FailE) as (l + arity) vs

compileCase :: [IxExpr]
            -- ^ Expressions to be matched
            -> IxExpr
            -- ^ Default fallthrough Expression
            -> [FnArmB IxExpr]
            -- ^ Remaining patterns, and their consequent expressions (case
            -- arms).
            -> IxExpr

compileCase [] d []                           = d
compileCase [] d ((FnArm _ [] e (Just p)):as) = embedIx $ IfEB p e (compileCase [] d as)
compileCase [] _ ((FnArm _ [] e Nothing):_)   = e
compileCase [] _ _ = error "compileCase: case expression, pattern counts mismatched."

compileCase (e:es) d as = foldr compileSection d
                        . pairSections
                        . groupByFstPat
                        $ as
  where
    compileSection :: ([FnArmB IxExpr], [FnArmB IxExpr])
                   -- ^ A pair of arms, the first with constructor patterns as
                   -- their first pattern, and the second with variable patterns.
                   -> IxExpr
                   -- ^ A fallthrough for this case section.
                   -> IxExpr

    -- A special case: if there are no constructor patterns, then the case
    -- expression is just a renaming, so we may discard it.
    compileSection ([], vcs) dix l vs
      | VarE u <- e l vs = compileCase es dix (stripLocalAlias (l - u) <$> vcs) l vs

    compileSection (ccs, vcs) dix l vs =
      let varCase  = compileVars vcs dix
          ctrCases = compileCtrs ccs
      in  embedCaseIx e (ctrCases ++ [varCase]) l vs

    -- | Group constructors by the shape of their outer pattern and recursively compile
    -- case expressions for the sub patterns.
    compileCtrs = map compileCtrGroup
                . groupBy ((==) `on` fstPatShape)
                . sortBy (comparing fstPatShape)

    -- | Create the case arm for the group of patterns sharing the same outer
    -- constructor.
    compileCtrGroup :: [FnArmB IxExpr] -> (SimplePatt, IxExpr)
    compileCtrGroup []        = error "compileCtrGroup: Empty Constructor Group"
    compileCtrGroup ccs@(c:_) =
      let pat       = fstPat c
          simplePat = simplify pat
          cix l vs =
            let patVs = map local [l - holes simplePat .. l - 1]
            in  compileCase (patVs ++ es) (close FallThroughE) (stripCtrPat <$> ccs) l vs
      in  (simplePat, cix)

    -- | Create the case arm for the variable cases.
    compileVars :: [FnArmB IxExpr] -> IxExpr -> (SimplePatt, IxExpr)
    compileVars [] dix  = (VarPB "_", dix)
    compileVars vcs dix =
      let cix l vs = compileCase es dix (stripLocalAlias (l - 1) <$> vcs) l vs
      in  (VarPB "_", cix)

    pairSections :: [[FnArmB IxExpr]] -> [([FnArmB IxExpr], [FnArmB IxExpr])]
    pairSections [] = []
    pairSections [xs]
      | isFstPatVar (head xs) = [([], xs)]
      | otherwise             = [(xs, [])]
    pairSections (xs:ys:xss)
      | isFstPatVar (head xs) = ([], xs) : pairSections (ys:xss)
      | otherwise             = (xs, ys) : pairSections xss

    groupByFstPat :: [FnArmB IxExpr] -> [[FnArmB IxExpr]]
    groupByFstPat = groupBy ((==) `on` isFstPatVar)

    -- | Pop the first pattern from the arm, as long as it is a variable
    -- pattern, and replace occurrences of the variable in the consequent
    -- expression with a local variable at a given level.
    stripLocalAlias :: Int -> FnArmB IxExpr -> FnArmB IxExpr
    stripLocalAlias lvl (FnArm i ((VarP v):ps) e' g) =
      let rn ix l vs = ix l (H.insert v lvl vs) in
        FnArm i ps (rn e') (rn <$> g)
    stripLocalAlias _ _ = error "stripVarPat: first pattern is not a variable"

    -- | Pop the first pattern from the arm, as long as it is a constructor
    -- pattern, and replace it with all of its sub-patterns.
    stripCtrPat :: FnArmB a -> FnArmB a
    stripCtrPat (FnArm i (p:ps) e' g) = FnArm i ((subPats p) ++ ps) e' g
    stripCtrPat _ = error "stripCtrPat: empty pattern list"

    embedCaseIx :: IxExpr -> [(SimplePatt, IxExpr)] -> IxExpr
    embedCaseIx eix aixs l vs = CaseE (eix l vs) (intoArm l vs <$> aixs)

    intoArm l vs (p, b)  = (p, b (l + holes p) vs)

    fstPat :: FnArmB a -> Patt
    fstPat (FnArm _ ps _ _) = head ps

    fstPatShape = simplify . fstPat

    isFstPatVar :: FnArmB a -> Bool
    isFstPatVar arm =
       case fstPat arm of
         (VarP _) -> True
         _        -> False
