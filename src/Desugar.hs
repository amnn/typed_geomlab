module Desugar where

import Control.Monad
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor.Foldable
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Expr
import Lexer
import Literal
import Patt
import Sugar
import Structure (shape)
import Token (Id)

desugarExpr :: Sugar -> Alex Expr
desugarExpr = cata d
  where
    d (VarSB x)         = var x
    d (AppSB x es)      = apply x es
    d (RangeSB f t)     = apply "_range" [f, t]
    d (LSectSB x e)     = apply "_lsect" [var x, e]
    d (RSectSB e x)     = apply "_rsect" [var x, e]
    d (IfSB c t e)      = IfE <$> c <*> t <*> e
    d (SeqSB a b)       = SeqE <$> a <*> b
    d (LitSB s)         = LitE <$> sequenceA s
    d (LetSB x a b)     = LetE x <$> a <*> b
    d (ListCompSB s gs) = compileListComp s gs nilB
    d (FnSB arms)       = sequence (map sequence arms) >>= compileFn

var :: Id -> Alex Expr
var = return . FreeE

apply :: Id -> [Alex Expr] -> Alex Expr
apply x es = AppE (FreeE x) <$> sequence es

compileListComp :: Alex Expr -> [GenB (Alex Expr)] -> Expr -> Alex Expr
compileListComp em []     acc = do { e <- em; return (consB e acc) }
compileListComp em (g:gs) acc = compileGen g acc (compileListComp em gs)
  where
    compileGen (FilterB pm) a inner = do
      p <- pm
      i <- inner a
      return (IfE p i a)
    compileGen (GenB p gm)  a inner = do
      gen <- gm
      anon <- genSym
      b <- genSym
      i <- inner (FreeE b)
      f <- compileFn [ FnArm "" [p,           (VarP b)] i         Nothing
                     , FnArm "" [(VarP anon), (VarP b)] (FreeE b) Nothing]
      return (AppE (FreeE "_mapa") [f, gen, a])

compileFn :: [FnArmB Expr] -> Alex Expr
compileFn []       = error "compileFn: No arms in function body."
compileFn as@(a:_) = do
  xs <- replicateM arity genSym
  e  <- compileCase (FreeE <$> xs) FailE as
  return (FnE xs e)
  where
    arity = let FnArm _ ps _ _ = a in length ps


compileCase :: [Expr] -> Expr -> [FnArmB Expr] -> Alex Expr
compileCase [] d []                           = return d
compileCase [] d ((FnArm _ [] e (Just p)):as) = compileCase [] d as >>= return . IfE p e
compileCase [] _ ((FnArm _ [] e Nothing):_)   = return e
compileCase [] _ _ = alexError "Shadowed pattern match"

compileCase (e:es) d as = foldr compileSection (return d)
                        . pairSections
                        . groupByFstPat
                        $ as
  where
    compileSection ([], vs) dm
      | FreeE u <- e = do
      dft   <- dm
      compileCase es dft (stripVarPat (rename u) <$> vs)

    compileSection (ctrs, vs) dm = do
      varCase  <- dm >>= compileVars vs
      ctrCases <- compileCtrs ctrs
      return $ CaseE e (ctrCases ++ [varCase])

    compileCtrs  = mapM compileCtrGroup
                 . groupBy ((==) `on` fstPatShape)
                 . sortBy (comparing fstPatShape)

    compileCtrGroup :: [FnArmB Expr] -> Alex (SimplePatt, Expr)
    compileCtrGroup []       = error "compileCtrGroup: Empty Constructor Group"
    compileCtrGroup cs@(c:_) = do
      pat  <- mapM (const genSym) . project . fstPat $ c
      let es' = (FreeE <$> toList pat) ++ es
      cases <- compileCase es' FallThroughE (stripCtrPat <$> cs)
      return (pat, cases)

    compileVars :: [FnArmB Expr] -> Expr -> Alex (SimplePatt, Expr)
    compileVars [] dft = do { a <- genSym; return (VarPB a, dft) }
    compileVars vs dft = do
      v <- genSym
      cases <- compileCase es dft (stripVarPat (rename v) <$> vs)
      return (VarPB v, cases)

    rename :: Id -> Id -> Id -> Maybe Expr
    rename v w u
      | w == u    = Just (FreeE v)
      | otherwise = Nothing

    pairSections :: [[FnArmB Expr]] -> [([FnArmB Expr], [FnArmB Expr])]
    pairSections [] = []
    pairSections [xs]
      | isFstPatVar (head xs) = [([], xs)]
      | otherwise             = [(xs, [])]
    pairSections (xs:ys:xss)
      | isFstPatVar (head xs) = ([], xs) : pairSections (ys:xss)
      | otherwise             = (xs, ys) : pairSections xss

    groupByFstPat :: [FnArmB Expr] -> [[FnArmB Expr]]
    groupByFstPat = groupBy ((==) `on` isFstPatVar)

    stripVarPat :: (Id -> Id -> Maybe Expr) -> FnArmB Expr -> FnArmB Expr
    stripVarPat tr (FnArm i ((VarP p):ps) e' g) =
      let rn = sub (tr p) in FnArm i ps (rn e') (rn <$> g)
    stripVarPat _ _ = error "stripVarPat: first pattern is not a variable"

    stripCtrPat :: FnArmB Expr -> FnArmB Expr
    stripCtrPat (FnArm i (p:ps) e' g) = FnArm i ((subPats p) ++ ps) e' g
    stripCtrPat _ = error "stripCtrPat: empty pattern list"

    fstPat :: FnArmB Expr -> Patt
    fstPat (FnArm _ ps _ _) = head ps

    fstPatShape = shape . project . fstPat

    isFstPatVar :: FnArmB Expr -> Bool
    isFstPatVar arm =
       case fstPat arm of
         (VarP _) -> True
         _        -> False
