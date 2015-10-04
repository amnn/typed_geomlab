{-# LANGUAGE TypeFamilies, DeriveFunctor #-}

module Expr where

import Prelude hiding (Foldable)
import Data.Functor.Foldable
import Literal
import Patt
import Token (Id)

data Expr = LitE (LitB Expr)
          | VarE Id
          | IfE Expr Expr Expr
          | FnE [FnArmB Expr]
          | AppE Expr [Expr]
          | LetE Id Expr Expr
          | SeqE Expr Expr
            deriving (Eq, Show)

data ExprB a = LitEB (LitB a)
             | VarEB Id
             | IfEB a a a
             | FnEB [FnArmB a]
             | AppEB a [a]
             | LetEB Id a a
             | SeqEB a a
               deriving (Eq, Show, Functor)

instance EmbedsLit Expr where
  embedLit = LitE

type instance Base Expr = ExprB
instance Foldable Expr where
  project (LitE s)     = LitEB s
  project (VarE x)     = VarEB x
  project (IfE c t e)  = IfEB c t e
  project (FnE as)     = FnEB as
  project (AppE f xs)  = AppEB f xs
  project (LetE x a b) = LetEB x a b
  project (SeqE a b)   = SeqEB a b
