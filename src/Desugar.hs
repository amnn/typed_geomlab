{-# LANGUAGE TupleSections #-}
module Desugar where

import           Control.Applicative   ((<|>))
import           Data.Expr
import           Data.Function         (on)
import           Data.Functor.Foldable
import qualified Data.HashMap          as H
import           Data.List             (groupBy, sortBy)
import           Data.Literal
import           Data.Maybe            (catMaybes)
import           Data.Ord              (comparing)
import           Data.Patt
import           Data.Sugar
import           Data.Token            (Id)

-- | A floating @ a @ term, containing de Bruijn indices, waiting to be placed
-- within a particular scope.
type Ix a = Int
         -- ^ The level at which this floating term is being inserted.
         -> H.Map Id Int
         -- ^ A mapping from the identifiers of currently bound variables to the
         -- levels in the AST they were bound at.
         -> a

desugarExpr :: Sugar -> Expr
desugarExpr s = cata d s 0 H.empty
  where
    d :: Base Sugar (Ix Expr)-> Ix Expr
    d (VarSB x)         = var x
    d (AppSB x es)      = apply x es
    d (RangeSB f t)     = apply "_range" [f, t]
    d (LSectSB e x)     = apply "_lsect" [var x, e]
    d (RSectSB x e)     = apply "_rsect" [var x, e]
    d (IfSB c t e)      = ifIx c t e
    d (LocSB lbl le)    = embedIx $ LocEB lbl le
    d (LitSB l)         = embedIx $ LitEB l
    d (LetSB x a b)     = letIx x a b
    d (ListCompSB e gs) = compileListComp e gs (close nilB)
    d (FnSB arms)       = compileFn arms

-- | Convert an expression with floating children into a floating expression
-- itself.
embedIx :: Base Expr (Ix Expr) -> Ix Expr
embedIx e l vs = embed $ (\ix -> ix l vs) <$> e

-- | Close over an expression with no dangling bound variables.
close :: a -> Ix a
close e _ _ = e

-- | Creating a floating expression that will reference a variable introduced at
-- level @ l' @ when reified.
local :: Int -> Ix Expr
local l' l _ = VarE (l - l')

-- | Convert a variable in the source language to potentially a de Bruijn
-- index, if the variable with that identifier appears bound within the scope
-- this fragment is reified in.
var :: Id -> Ix Expr
var v l vs =
  case H.lookup v vs of
    Just l' -> VarE (l - l')
    Nothing -> FreeE v

-- | Desugar an if expression into a case expression with boolean
-- patterns. Boolean patterns are not a source language feature, but are used
-- internally.
ifIx :: Ix Expr
     -- ^ Desugared Condition Expression
     -> Ix Expr
     -- ^ Desugared Then Branch
     -> Ix Expr
     -- ^ Desugared Else Branch
     -> Ix Expr
ifIx c t e = embedIx $ ifEB c t e

-- | Convert a function application in the source language into one in the
-- desugared language.
apply :: Id -> [Ix Expr] -> Ix Expr
apply x = embedIx . AppEB (var x)

-- | Desugaring of (possibly recursive) let expressions. This is not a simple
-- embedding because a @ let @ expression introduces a variable.
letIx :: Id -> Ix Expr -> Ix Expr -> Ix Expr
letIx x a b l vs = LetE (a l' vs') (b l' vs')
  where
    l'  = l + 1
    vs' = H.insert x l vs

compileListComp :: Ix Expr
                -- ^ The element expression
                -> [GenB (Ix Expr)]
                -- ^ The list of generators
                -> Ix Expr
                -- ^ The tail of the result
                -> Ix Expr

compileListComp eix []     aix = embedIx (LitEB (ConsB eix aix))
compileListComp eix (g:gs) aix = compileGen g aix (compileListComp eix gs)
  where
    compileGen :: GenB (Ix Expr)
               -- ^ A generator
               -> Ix Expr
               -- ^ The tail of the result
               -> (Ix Expr -> Ix Expr)
               -- ^ A continuation, to generate the inner loop of the generator
               -> Ix Expr
    compileGen (FilterB pix) bix inner = ifIx pix (inner bix) bix
    compileGen (GenB p gix)  bix inner = \ l vs ->
      let cix = local (l+1)
          iix = inner cix
          fix = compileFn [ FnArm "" [p,          (VarP "_")] iix Nothing
                          , FnArm "" [(VarP "_"), (VarP "_")] cix Nothing
                          ]
      in apply "_mapa" [fix, gix, bix] l vs

compileFn :: [FnArmB (Ix Expr)] -> Ix Expr
compileFn []       _ _  = error "compileFn: No arms in function body."
compileFn as@(a:_) l vs = FnE arity body
  where
    arity = let FnArm _ ps _ _ = a in length ps
    xs    = map local [l .. l + arity - 1]
    body  = case compileCase xs (close Nothing) as (l + arity) vs of
              Nothing -> error "compileFn: Empty case expression!"
              Just b  -> b

compileCase :: [Ix Expr]
            -- ^ Expressions to be matched
            -> Ix (Maybe Expr)
            -- ^ Potential fallthrough expression
            -> [FnArmB (Ix Expr)]
            -- ^ Remaining patterns, and their consequent expressions (case
            -- arms).
            -> Ix (Maybe Expr)

compileCase [] d [] = d

compileCase [] d ((FnArm _ [] eix (Just pix)):as) = \l vs ->
  let p      = pix l vs
      e      = eix l vs
      ifE    = embed . ifEB p e
      guardE = Just . embed $ guardEB p e
  in (ifE <$> compileCase [] d as l vs) <|> guardE

compileCase [] _ ((FnArm _ [] e Nothing):_) = \l vs -> Just (e l vs)
compileCase [] _ _ = error "compileCase: case expression, pattern counts mismatched."

compileCase (e:es) d as = foldr compileSection d
                        . pairSections
                        . groupByFstPat
                        $ as
  where
    compileSection :: ([FnArmB (Ix Expr)], [FnArmB (Ix Expr)])
                   -- ^ A pair of arms, the first with constructor patterns as
                   -- their first pattern, and the second with variable
                   -- patterns.
                   -> Ix (Maybe Expr)
                   -- ^ A potential fallthrough for this case section.
                   -> Ix (Maybe Expr)

    -- A special case: if there are no constructor patterns, then the case
    -- expression is just a renaming, so we may discard it.
    compileSection ([], vcs) dmix l vs | VarE u <- e l vs =
      compileCase es dmix (stripLocalAlias (l - u) <$> vcs) l vs

    compileSection (ccs, vcs) dmix l vs =
      let varCase  = compileVars vcs dmix
          ctrCases = compileCtrs ccs
      in  Just $ embedCaseIx e (ctrCases ++ [varCase]) l vs

    -- | Group constructors by the shape of their outer pattern and recursively
    -- compile case expressions for the sub patterns.
    compileCtrs = map compileCtrGroup
                . groupBy ((==) `on` fstPatShape)
                . sortBy  (comparing fstPatShape)

    -- | Create the case arm for the group of patterns sharing the same outer
    -- constructor.
    compileCtrGroup :: [FnArmB (Ix Expr)] -> Ix (Maybe (SimplePatt, Expr))
    compileCtrGroup []        = error "compileCtrGroup: Empty Constructor Group"
    compileCtrGroup ccs@(c:_) =
      let pat       = fstPat c
          simplePat = simplify pat
          cmix l vs =
            let patVs = map local [l - holes simplePat .. l - 1]
            in  compileCase (patVs ++ es) (close Nothing) (stripCtrPat <$> ccs) l vs
      in withPatt simplePat cmix

    -- | Create the case arm for the variable cases.
    compileVars :: [FnArmB (Ix Expr)]
                -> Ix (Maybe Expr)
                -> Ix (Maybe (SimplePatt, Expr))
    compileVars []  dmix = withPatt (VarPB "_") dmix
    compileVars vcs dmix =
      let cmix l vs = compileCase es dmix (stripLocalAlias (l - 1) <$> vcs) l vs
      in  withPatt (VarPB "_") cmix

    pairSections :: [[FnArmB (Ix Expr)]]
                 -> [([FnArmB (Ix Expr)], [FnArmB (Ix Expr)])]
    pairSections [] = []
    pairSections [xs]
      | isFstPatVar (head xs) = [([], xs)]
      | otherwise             = [(xs, [])]
    pairSections (xs:ys:xss)
      | isFstPatVar (head xs) = ([], xs) : pairSections (ys:xss)
      | otherwise             = (xs, ys) : pairSections xss

    groupByFstPat :: [FnArmB (Ix Expr)] -> [[FnArmB (Ix Expr)]]
    groupByFstPat = groupBy ((==) `on` isFstPatVar)

    -- | Pop the first pattern from the arm, as long as it is a variable
    -- pattern, and replace occurrences of the variable in the consequent
    -- expression with a local variable at a given level.
    stripLocalAlias :: Int -> FnArmB (Ix Expr) -> FnArmB (Ix Expr)
    stripLocalAlias lvl (FnArm i ((VarP v):ps) e' g) =
      let rn ix l vs = ix l (H.insert v lvl vs) in
        FnArm i ps (rn e') (rn <$> g)
    stripLocalAlias _ _ = error "stripVarPat: first pattern is not a variable"

    -- | Pop the first pattern from the arm, as long as it is a constructor
    -- pattern, and replace it with all of its sub-patterns.
    stripCtrPat :: FnArmB a -> FnArmB a
    stripCtrPat (FnArm i (p:ps) e' g) = FnArm i ((subPats p) ++ ps) e' g
    stripCtrPat _ = error "stripCtrPat: empty pattern list"

    embedCaseIx :: Ix Expr -> [Ix (Maybe (SimplePatt, Expr))] -> Ix Expr
    embedCaseIx eix amixs l vs = CaseE (eix l vs) (catMaybes ((\mix -> mix l vs) <$> amixs))

    withPatt :: SimplePatt -> Ix (Maybe Expr) -> Ix (Maybe (SimplePatt, Expr))
    withPatt p emix l vs = (p,) <$> emix (l + holes p) vs

    fstPat :: FnArmB a -> Patt
    fstPat (FnArm _ ps _ _) = head ps

    fstPatShape = simplify . fstPat

    isFstPatVar :: FnArmB a -> Bool
    isFstPatVar arm =
       case fstPat arm of
         (VarP _) -> True
         _        -> False
