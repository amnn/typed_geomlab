{-# LANGUAGE TypeFamilies
           , DeriveFoldable
           , DeriveFunctor
           , DeriveTraversable
           , FlexibleInstances
           , PatternGuards #-}

module Type where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor.Foldable
import qualified Data.HashMap as H
import Data.List (intercalate)
import Data.Traversable (mapAccumL)
import Structure (shapeEq)
import Token (Id)

data Ty v    = BoolT | NumT | StrT | AtomT | VarT v
             | ListT (Ty v) | ArrT [Ty v] (Ty v)
               deriving Eq

data TyB v a = BoolTB | NumTB | StrTB | AtomTB | VarTB v
             | ListTB a | ArrTB [a] a
               deriving (Eq, Show, P.Foldable, Functor, Traversable)

type instance Base (Ty v) = TyB v
instance Foldable (Ty v) where
  project BoolT       = BoolTB
  project NumT        = NumTB
  project StrT        = StrTB
  project AtomT       = AtomTB
  project (VarT v)    = VarTB v
  project (ListT t)   = ListTB t
  project (ArrT as a) = ArrTB as a

instance Show (Ty Id) where
  show BoolT       = "bool"
  show NumT        = "num"
  show StrT        = "str"
  show AtomT       = "atom"
  show (VarT x)    = "'" ++ x
  show (ListT t)   = "[" ++ show t ++ "]"
  show (ArrT as b) = showFormals as ++ " -> " ++ show b
    where
      showFormals [f]   = wrap f
      showFormals fs    = "(" ++  intercalate ", " (map show fs) ++ ")"

wrap :: Ty Id -> String
wrap t@(ArrT _ _) = "(" ++ show t ++ ")"
wrap t            = show t

alphaEq :: Ty Id -> Ty Id -> Bool
alphaEq t u = snd $ eq H.empty t u
  where
    eq subst (VarT v) (VarT w)
      | Just x <- H.lookup v subst = (subst, x == w)
      | otherwise                  = (H.insert v w subst, True)

    eq subst v w | pv `shapeEq` pw = (subst', and vs)
      where
       (subst', vs) = mapAccumL (uncurry . eq) subst $ (zip `on` toList) pv pw
       [pv, pw]     = project <$> [v, w]

    eq subst _ _  = (subst, False)
