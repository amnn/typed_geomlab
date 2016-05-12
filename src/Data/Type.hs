{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Type where

import           Data.Foldable         (toList)
import           Data.Function         (on)
import           Data.Functor.Foldable
import qualified Data.HashMap.Strict   as H
import           Data.List             (intercalate)
import           Data.Structure        (shapeEq)
import           Data.Token            (Id)
import           Data.Traversable      (mapAccumL)
import           Prelude               hiding (Foldable)
import qualified Prelude               as P (Foldable)

data Ty = BoolT | NumT | StrT | AtomT | VarT Id
        | TagT Id | NilT | ConsT Ty Ty | ArrT [Ty] Ty
          deriving Eq

data TyB a = BoolTB | NumTB | StrTB | AtomTB | VarTB Id
           | TagTB Id | NilTB | ConsTB Ty Ty | ArrTB [a] a
               deriving (Eq, Show, P.Foldable, Functor, Traversable)

type instance Base Ty = TyB
instance Foldable Ty where
  project  BoolT      = BoolTB
  project  NumT       = NumTB
  project  StrT       = StrTB
  project  AtomT      = AtomTB
  project (VarT v)    = VarTB v
  project (TagT t)    = TagTB t
  project  NilT       = NilTB
  project (ConsT a b) = ConsTB a b
  project (ArrT as a) = ArrTB as a

instance Show Ty where
  show  BoolT      = "bool"
  show  NumT       = "num"
  show  StrT       = "str"
  show  AtomT      = "atom"
  show (VarT x)    = "'" ++ x
  show  NilT       = "[]"
  show (TagT t)    = "#" ++ t

  show (ConsT a b) = wrap a ++ ":" ++ showTail b
    where
      showTail t@(ConsT _ _) = show t
      showTail t             = wrap t

  show (ArrT as b) = showFormals as ++ " -> " ++ show b
    where
      showFormals [f]   = wrap f
      showFormals fs    = "(" ++  intercalate ", " (map show fs) ++ ")"

wrap :: Ty -> String
wrap t@(ArrT _ _)  = "(" ++ show t ++ ")"
wrap t@(ConsT _ _) = "(" ++ show t ++ ")"
wrap t             = show t

alphaEq :: Ty -> Ty -> Bool
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
