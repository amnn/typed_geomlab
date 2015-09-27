module Expr where

import Literal
import Patt
import Token (Id)

data Expr = LitE (LitShape Expr)
          | VarE Id
          | IfE Expr Expr Expr
          | FnE [Id] Expr
          | AppE Id [Expr]
          | LetE Id Expr Expr
          | SeqE Expr Expr
            deriving (Eq, Show)
