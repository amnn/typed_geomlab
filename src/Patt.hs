{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Patt where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Functor.Foldable
import Literal
import Token (Id)

data Patt = ValP (LitB Patt)
          | AnonP
          | VarP Id
          | OffsetP Patt Double
            deriving (Eq, Show)

data PattB a = ValPB (LitB a)
             | AnonPB
             | VarPB Id
             | OffsetPB a Double
               deriving (Eq, Show, Functor)

data GenB a = GenB Patt a | FilterB a
              deriving (Eq, Show, Functor)

data FnArmB a = FnArm Id [Patt] a (Maybe a)
                deriving ( Eq
                         , Show
                         , Functor
                         , P.Foldable
                         , Traversable
                         )

type instance Base Patt = PattB
instance Foldable Patt where
  project (ValP s)      = ValPB s
  project AnonP         = AnonPB
  project (VarP x)      = VarPB x
  project (OffsetP p n) = OffsetPB p n

instance EmbedsLit Patt where
  embedLit = ValP
