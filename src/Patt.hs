{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Patt where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Foldable (toList)
import Data.Functor.Foldable
import Literal
import Token (Id)

data Patt = ValP (LitB Patt)
          | VarP Id
          | CtrP Id [Patt]
          | OffsetP Patt Double
            deriving (Eq, Show)

data PattB a = ValPB (LitB a)
             | VarPB Id
             | CtrPB Id [a]
             | OffsetPB a Double
               deriving ( Eq
                        , Ord
                        , Show
                        , P.Foldable
                        , Functor
                        , Traversable
                        )

data GenB a = GenB Patt a | FilterB a
              deriving (Eq, Show, Functor)

data FnArmB a = FnArm Id [Patt] a (Maybe a)
                deriving ( Eq
                         , Show
                         , Functor
                         , P.Foldable
                         , Traversable
                         )

type SimplePatt = PattB Id
type SimplePattShape = PattB ()

type instance Base Patt = PattB
instance Foldable Patt where
  project (ValP s)      = ValPB s
  project (VarP x)      = VarPB x
  project (CtrP c ps)   = CtrPB c ps
  project (OffsetP p n) = OffsetPB p n

instance EmbedsLit Patt where
  embedLit = ValP


isVar :: PattB a -> Bool
isVar (VarPB _) = True
isVar _         = False

patVars :: SimplePatt -> [Id]
patVars (VarPB i) = [i]
patVars p         = toList p

subPats :: Patt -> [Patt]
subPats = toList . project
