{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
module Type where

import Prelude hiding (Foldable)
import Data.Functor.Foldable
import Data.List (intercalate)
import Token (Id)

data Ty = BoolT | NumT | StrT | AtomT | VarT Id
        | ListT Ty | AppT [Ty] Ty
          deriving (Eq)

data TyB a = BoolTB | NumTB | StrTB | AtomTB | VarTB Id
           | ListTB a | AppTB [a] a
             deriving (Eq, Show, Functor)


type instance Base Ty = TyB
instance Foldable Ty where
  project BoolT       = BoolTB
  project NumT        = NumTB
  project StrT        = StrTB
  project AtomT       = AtomTB
  project (VarT x)    = VarTB x
  project (ListT t)   = ListTB t
  project (AppT as b) = AppTB as b

instance Show Ty where
  show BoolT       = "bool"
  show NumT        = "num"
  show StrT        = "str"
  show AtomT       = "atom"
  show (VarT x)    = "'" ++ x
  show (ListT t)   = "[" ++ show t ++ "]"
  show (AppT as b) = showFormals as ++ " -> " ++ show b
    where
      showFormals [f] = show f
      showFormals fs = "(" ++  intercalate ", " (map show fs) ++ ")"
