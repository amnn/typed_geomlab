module Desugar where

import Control.Monad
import Data.Functor.Foldable
import Expr
import Lexer
import Literal
import Patt
import Sugar
import Token (Id)

desugarExpr :: Sugar -> Alex Expr
desugarExpr = cata d
  where
    d (LitSB s)         = liftM LitE (sequenceA s)
    d (ListCompSB s gs) = expand s gs nilB
    d (RangeSB f t)     = apply "_range" [f, t]
    d (VarSB x)         = return (VarE x)
    d (IfSB c t e)      = liftM3 IfE c t e
    d (FnSB arms)       = liftM FnE (mapM sequenceA arms)
    d (AppSB x es)      = apply x es
    d (LSectSB x e)     = apply "_lsect" [var x, e]
    d (RSectSB e x)     = apply "_rsect" [var x, e]
    d (LetSB x a b)     = liftM2 (LetE x) a b
    d (SeqSB a b)       = liftM2 SeqE a b

var :: Id -> Alex Expr
var = return . VarE

apply :: Id -> [Alex Expr] -> Alex Expr
apply x es = AppE (VarE x) <$> sequence es

expand :: Alex Expr -> [GenB (Alex Expr)] -> Expr -> Alex Expr
expand em []     a = do { e <- em; return (consB e a) }
expand em (g:gs) a = compileGen g a (expand em gs)
  where
    compileGen (FilterB pm) a inner = do
      p <- pm
      i <- inner a
      return (IfE p i a)
    compileGen (GenB p gm)  a inner = do
      g <- gm
      b <- genSym
      i <- inner (VarE b)
      let f = FnE [ FnArm "" [p,     (VarP b)] i        Nothing
                  , FnArm "" [AnonP, (VarP b)] (VarE b) Nothing]
      return (AppE (VarE "_mapa") [f, g, a])
