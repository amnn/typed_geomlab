module Desugar where

import Data.Functor.Foldable
import Expr
import Sugar
import Token (Id)

desugar :: Sugar -> Expr
desugar = cata desugarer
  where
    desugarer (LitSB s)         = LitE s
    desugarer (ListCompSB s gs) = undefined
    desugarer (RangeSB f t)     = apply "_range" [f, t]
    desugarer (VarSB x)         = VarE x
    desugarer (IfSB c t e)      = IfE c t e
    desugarer (FnSB arms)       = undefined
    desugarer (AppSB x es)      = apply x es
    desugarer (LSectSB x e)     = apply "_lsect" [VarE x, e]
    desugarer (RSectSB e x)     = apply "_rsect" [VarE x, e]
    desugarer (LetSB x a b)     = LetE x a b
    desugarer (SeqSB a b)       = SeqE a b

apply :: Id -> [Expr] -> Expr
apply x es = AppE (VarE x) es
