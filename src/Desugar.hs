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

type IxExpr = Int -> H.Map Id Int -> Expr

desugarExpr :: Sugar -> Expr
desugarExpr s = cata d s 0 H.empty
  where
    d :: Base Sugar IxExpr -> IxExpr
    d (VarSB x)         = var x
    d (AppSB x es)      = apply x es
    d (RangeSB f t)     = apply "_range" [f, t]
    d (LSectSB x e)     = apply "_lsect" [var x, e]
    d (RSectSB e x)     = apply "_rsect" [var x, e]
    d (IfSB c t e)      = reifyIx $ IfEB c t e
    d (SeqSB a b)       = reifyIx $ SeqEB a b
    d (LitSB l)         = reifyIx $ LitEB l
    d (LetSB x a b)     = letIx x a b
    d (ListCompSB e gs) = compileListComp e gs (close nilB)
    d (FnSB arms)       = compileFn arms

liftIx :: Traversable t => (t Expr -> Expr) -> t IxExpr -> IxExpr
liftIx f e l vs = f $ (\ix -> ix l vs) <$> e

reifyIx :: Base Expr IxExpr -> IxExpr
reifyIx = liftIx embed

close :: Expr -> IxExpr
close e _ _ = e

local :: Int -> IxExpr
local l' l _ = VarE (l - l')

var :: Id -> IxExpr
var v l vs =
  case H.lookup v vs of
    Just l' -> VarE (l - l')
    Nothing -> FreeE v

apply :: Id -> [IxExpr] -> IxExpr
apply x xs = reifyIx (AppEB (var x) xs)

letIx :: Id -> IxExpr -> IxExpr -> IxExpr
letIx x a b l vs = LetE (a l' vs') (b l' vs')
  where
    l'  = l + 1
    vs' = H.insert x l vs

compileListComp :: IxExpr -> [GenB IxExpr] -> IxExpr -> IxExpr
compileListComp eix []     aix = reifyIx (LitEB (ConsB eix aix))
compileListComp eix (g:gs) aix = compileGen g aix (compileListComp eix gs)
  where
    compileGen :: GenB IxExpr -> IxExpr -> (IxExpr -> IxExpr) -> IxExpr
    compileGen (FilterB pix) bix inner = reifyIx (IfEB pix (inner bix) bix)
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

compileCase :: [IxExpr] -> IxExpr -> [FnArmB IxExpr] -> IxExpr
compileCase [] d []                           = d
compileCase [] d ((FnArm _ [] e (Just p)):as) = reifyIx $ IfEB p e (compileCase [] d as)
compileCase [] _ ((FnArm _ [] e Nothing):_)   = e
compileCase [] _ _ = error "compileCase: case expression, pattern counts mismatched."

compileCase (e:es) d as = foldr compileSection d
                        . pairSections
                        . groupByFstPat
                        $ as
  where
    compileSection :: ([FnArmB IxExpr], [FnArmB IxExpr]) -> IxExpr -> IxExpr
    compileSection ([], vcs) dix l vs
      | VarE u <- e l vs = compileCase es dix (stripLocalAlias (l - u) <$> vcs) l vs

    compileSection (ccs, vcs) dix l vs =
      let varCase  = compileVars vcs dix
          ctrCases = compileCtrs ccs
      in  reifyCase e (ctrCases ++ [varCase]) l vs

    compileCtrs = map compileCtrGroup
                . groupBy ((==) `on` fstPatShape)
                . sortBy (comparing fstPatShape)

    compileCtrGroup :: [FnArmB IxExpr] -> (SimplePatt, IxExpr)
    compileCtrGroup []        = error "compileCtrGroup: Empty Constructor Group"
    compileCtrGroup ccs@(c:_) =
      let pat       = fstPat c
          simplePat = simplify pat
          cix l vs =
            let patVs = map local [l - holes simplePat .. l - 1]
            in  compileCase (patVs ++ es) (close FallThroughE) (stripCtrPat <$> ccs) l vs
      in  (simplePat, cix)

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

    stripLocalAlias :: Int -> FnArmB IxExpr -> FnArmB IxExpr
    stripLocalAlias lvl (FnArm i ((VarP v):ps) e' g) =
      let rn ix l vs = ix l (H.insert v lvl vs) in
        FnArm i ps (rn e') (rn <$> g)
    stripLocalAlias _ _ = error "stripVarPat: first pattern is not a variable"

    stripCtrPat :: FnArmB a -> FnArmB a
    stripCtrPat (FnArm i (p:ps) e' g) = FnArm i ((subPats p) ++ ps) e' g
    stripCtrPat _ = error "stripCtrPat: empty pattern list"

    reifyCase :: IxExpr -> [(SimplePatt, IxExpr)] -> IxExpr
    reifyCase eix aixs l vs = CaseE (eix l vs) (intoArm l vs <$> aixs)

    intoArm l vs (p, b)  = (p, b (l + holes p) vs)

    fstPat :: FnArmB a -> Patt
    fstPat (FnArm _ ps _ _) = head ps

    fstPatShape = simplify . fstPat

    isFstPatVar :: FnArmB a -> Bool
    isFstPatVar arm =
       case fstPat arm of
         (VarP _) -> True
         _        -> False
