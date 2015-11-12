{-# LANGUAGE TypeFamilies, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances #-}

module Type where

import Prelude hiding (Foldable)
import Data.Foldable
import Data.List (intercalate)
import Token (Id)

data TyB v a = BoolTB | NumTB | StrTB | AtomTB | VarTB v
             | ListTB a | ArrTB [a] a
               deriving (Eq, Show, Foldable, Functor, Traversable)

newtype FixTy = FixTy { unfixTy :: TyB Id FixTy }
instance Show FixTy where
  show (FixTy BoolTB)       = "bool"
  show (FixTy NumTB)        = "num"
  show (FixTy StrTB)        = "str"
  show (FixTy AtomTB)       = "atom"
  show (FixTy (VarTB x))    = "'" ++ x
  show (FixTy (ListTB t))   = "[" ++ show t ++ "]"
  show (FixTy (ArrTB as b)) = showFormals as ++ " -> " ++ show b
    where
      showFormals [f] = show f
      showFormals fs = "(" ++  intercalate ", " (map show fs) ++ ")"
