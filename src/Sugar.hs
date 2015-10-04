{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Sugar where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Functor.Foldable
import Literal
import Patt
import Token (Id)

type Gen   = GenB Sugar
type FnArm = FnArmB Sugar

data Decl = Decl Id Sugar
            deriving (Eq, Show)

data Sugar = LitS (LitB Sugar)
           | ListCompS Sugar [Gen]
           | RangeS Sugar Sugar
           | VarS Id
           | IfS Sugar Sugar Sugar
           | FnS [FnArm]
           | AppS Id [Sugar]
           | LSectS Id Sugar
           | RSectS Sugar Id
           | LetS Id Sugar Sugar
           | SeqS Sugar Sugar
             deriving (Eq, Show)

data SugarB a = LitSB (LitB a)
              | ListCompSB a [GenB a]
              | RangeSB a a
              | VarSB Id
              | IfSB a a a
              | FnSB [FnArmB a]
              | AppSB Id [a]
              | LSectSB Id a
              | RSectSB a Id
              | LetSB Id a a
              | SeqSB a a
                deriving (Eq, Show, Functor)

data Para a = Def Id a | Eval a
              deriving (Eq
                       , Show
                       , Functor
                       , P.Foldable
                       , Traversable)

instance EmbedsLit Sugar where
  embedLit = LitS

declToDef :: Decl -> Para Sugar
declToDef (Decl id e) = Def id e

declToLet :: Decl -> Sugar -> Sugar
declToLet (Decl id e) = LetS id e

type instance Base Sugar = SugarB
instance Foldable Sugar where
  project (LitS s)         = LitSB s
  project (ListCompS s gs) = ListCompSB s gs
  project (RangeS from to) = RangeSB from to
  project (VarS x)         = VarSB x
  project (IfS c t e)      = IfSB c t e
  project (FnS arms)       = FnSB arms
  project (AppS f xs)      = AppSB f xs
  project (LSectS f x)     = LSectSB f x
  project (RSectS x f)     = RSectSB x f
  project (LetS x a b)     = LetSB x a b
  project (SeqS a b)       = SeqSB a b
