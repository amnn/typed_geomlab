{-# LANGUAGE TypeFamilies, DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Expr where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Functor.Foldable
import Literal
import Patt
import Token (Id)

data Expr = LitE (LitB Expr)
          | VarE !Int
          | FreeE Id
          | IfE Expr Expr Expr
          | CaseE Expr [(SimplePatt, Expr)]
          | FnE !Int Expr
          | AppE Expr [Expr]
          | LetE Expr Expr
          | SeqE Expr Expr

          -- Case Expression Specific
          | FailE
          | FallThroughE
            deriving (Eq, Show)

data ExprB a = LitEB (LitB a)
             | VarEB !Int
             | FreeEB Id
             | IfEB a a a
             | CaseEB a [(SimplePatt, a)]
             | FnEB !Int a
             | AppEB a [a]
             | LetEB a a
             | SeqEB a a
             | FailEB
             | FallThroughEB
               deriving ( Eq, Show
                        , P.Foldable
                        , Traversable
                        , Functor)

instance EmbedsLit Expr where
  embedLit = LitE

type instance Base Expr = ExprB
instance Foldable Expr where
  project (LitE s)     = LitEB s
  project (VarE x)     = VarEB x
  project (FreeE x)    = FreeEB x
  project (IfE c t e)  = IfEB c t e
  project (CaseE e as) = CaseEB e as
  project (FnE n e)    = FnEB n e
  project (AppE f xs)  = AppEB f xs
  project (LetE a b)   = LetEB a b
  project (SeqE a b)   = SeqEB a b
  project FailE        = FailEB
  project FallThroughE = FallThroughEB

instance Unfoldable Expr where
  embed (LitEB s)     = LitE s
  embed (VarEB x)     = VarE x
  embed (FreeEB x)    = FreeE x
  embed (IfEB c t e)  = IfE c t e
  embed (CaseEB e as) = CaseE e as
  embed (FnEB n e)    = FnE n e
  embed (AppEB f xs)  = AppE f xs
  embed (LetEB a b)   = LetE a b
  embed (SeqEB a b)   = SeqE a b
  embed FailEB        = FailE
  embed FallThroughEB = FallThroughE
