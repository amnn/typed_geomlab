{-# LANGUAGE TypeFamilies
           , DeriveFoldable
           , DeriveFunctor
           , DeriveTraversable
           , FlexibleInstances
           , PatternGuards #-}

module Type where

import Prelude hiding (Foldable)
import Data.Foldable
import Data.Function (on)
import qualified Data.HashMap as H
import Data.List (intercalate)
import Data.Traversable (mapAccumL)
import Structure (shapeEq)
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

alphaEq :: FixTy -> FixTy -> Bool
alphaEq t u = snd $ eq H.empty t u
  where
    eq subst (FixTy (VarTB v)) (FixTy (VarTB w))
      | Just x <- H.lookup v subst = (subst, x == w)
      | otherwise                  = (H.insert v w subst, True)

    eq subst (FixTy v) (FixTy w)
      | v `shapeEq` w =
        let (subst', vs) = mapAccumL (uncurry . eq) subst $ (zip `on` toList) v w
        in (subst', and vs)
      | otherwise     = (subst, False)
