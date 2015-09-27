module Expr where

import Token (Id)
import Shape
import Patt

data GenLvl = GenLvl Patt Sugar (Maybe Sugar)
              deriving (Eq, Show)

data FnArm = FnArm Id [Patt] Sugar (Maybe Sugar)
             deriving (Eq, Show)

data Decl = Decl Id Sugar
            deriving (Eq, Show)

data Sugar = LitS (Shape Sugar)
           | ListCompS Sugar [GenLvl]
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

data Expr = LitE (Shape Expr)
          | VarE Id
          | IfE Expr Expr Expr
          | FnE [Id] Expr
          | AppE Id [Expr]
          | LetE Id Expr Expr
          | SeqE Expr Expr
            deriving (Eq, Show)

data Para a = Def Id a
            | Eval a
              deriving (Eq, Show)

instance HasShape Sugar where
  embedShape = LitS

declToDef :: Decl -> Para Sugar
declToDef (Decl id e) = Def id e

declToLet :: Decl -> Sugar -> Sugar
declToLet (Decl id e) = LetS id e
