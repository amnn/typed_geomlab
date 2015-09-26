module Expr where

import Token (Id)
import Shape
import Patt

data GenLvl = GenLvl Patt Expr (Maybe Expr)
              deriving (Eq, Show)

data FnArm = FnArm Id [Patt] Expr (Maybe Expr)
             deriving (Eq, Show)

data Decl = Decl Id Expr
            deriving (Eq, Show)

data Expr = LitE (Shape Expr)
          | ListCompE Expr [GenLvl]
          | RangeE Expr Expr
          | VarE Id
          | IfE Expr Expr Expr
          | FnE [FnArm]
          | AppE Id [Expr]
          | LSectE Id Expr
          | RSectE Expr Id
          | LetE Id Expr Expr
          | SeqE Expr Expr
            deriving (Eq, Show)

data Para = Def Id Expr
          | Eval Expr
            deriving (Eq, Show)

instance HasShape Expr where
  embedShape = LitE

declToDef :: Decl -> Para
declToDef (Decl id e) = Def id e

declToLet :: Decl -> Expr -> Expr
declToLet (Decl id e) = LetE id e
