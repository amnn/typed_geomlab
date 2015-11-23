{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Sugar where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Functor.Foldable
import Literal
import Location
import Patt
import Token (Id)

type Gen   = GenB Sugar
type FnArm = FnArmB Sugar

-- | An assignment of an expression to a variable, either at the top level, or
-- within a let expression.
data Decl = Decl Id Sugar
            deriving (Eq, Show)

-- | Faithful representation of the source language as an AST.
data Sugar = LitS (LitB Sugar)
           | ListCompS Sugar [Gen]
           | RangeS Sugar Sugar
           | VarS Id
           | IfS Sugar Sugar Sugar
           | FnS [FnArm]
           | AppS Id [Sugar]
           | LSectS Sugar Id
           | RSectS Id Sugar
           | LetS Id Sugar Sugar
           | SeqS Sugar Sugar
           | LocS (Located Sugar)
             deriving (Eq, Show)

-- | A functor whose least-fixed point is isomorphic to @ Sugar @.
data SugarB a = LitSB (LitB a)
              | ListCompSB a [GenB a]
              | RangeSB a a
              | VarSB Id
              | IfSB a a a
              | FnSB [FnArmB a]
              | AppSB Id [a]
              | LSectSB a Id
              | RSectSB Id a
              | LetSB Id a a
              | SeqSB a a
              | LocSB (Located a)
                deriving (Eq, Show, Functor)

-- | A top level statement, parametrised by its expression type.
data Para a = Def Id a | Eval a
              deriving (Eq
                       , Show
                       , Functor
                       , P.Foldable
                       , Traversable)

instance EmbedsLit Sugar where
  embedLit = LitS

-- | Convert a generic assignment to a top-level assignment.
declToDef :: Decl -> Para Sugar
declToDef (Decl x e) = Def x e

-- | Conversion from a generic assignment to a let expression.
declToLet :: Decl -> Sugar -> Sugar
declToLet (Decl x e) = LetS x e

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
  project (LocS ls)        = LocSB ls
