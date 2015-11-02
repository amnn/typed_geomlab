{-# LANGUAGE TypeFamilies, DeriveFoldable, DeriveFunctor #-}

module Expr where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor.Foldable
import qualified Data.HashMap as H
import Data.Maybe (fromMaybe)
import Literal
import Patt
import Structure
import Token (Id)

data Expr = LitE (LitB Expr)
          | VarE Int
          | FreeE Id
          | IfE Expr Expr Expr
          | CaseE Expr [(SimplePatt, Expr)]
          | FnE [Id] Expr
          | AppE Expr [Expr]
          | LetE Id Expr Expr
          | SeqE Expr Expr

          -- Case Expression Specific
          | FailE
          | FallThroughE
            deriving (Eq, Show)

data ExprB a = LitEB (LitB a)
             | VarEB Int
             | FreeEB Id
             | IfEB a a a
             | CaseEB a [(SimplePatt, a)]
             | FnEB [Id] a
             | AppEB a [a]
             | LetEB Id a a
             | SeqEB a a
             | FailEB
             | FallThroughEB
               deriving (Eq, Show, P.Foldable, Functor)

instance EmbedsLit Expr where
  embedLit = LitE

type instance Base Expr = ExprB
instance Foldable Expr where
  project (LitE s)     = LitEB s
  project (VarE x)     = VarEB x
  project (FreeE x)    = FreeEB x
  project (IfE c t e)  = IfEB c t e
  project (CaseE e as) = CaseEB e as
  project (FnE xs e)   = FnEB xs e
  project (AppE f xs)  = AppEB f xs
  project (LetE x a b) = LetEB x a b
  project (SeqE a b)   = SeqEB a b
  project FailE        = FailEB
  project FallThroughE = FallThroughEB

instance Unfoldable Expr where
  embed (LitEB s)     = LitE s
  embed (VarEB x)     = VarE x
  embed (FreeEB x)    = FreeE x
  embed (IfEB c t e)  = IfE c t e
  embed (CaseEB e as) = CaseE e as
  embed (FnEB xs e)   = FnE xs e
  embed (AppEB f xs)  = AppE f xs
  embed (LetEB x a b) = LetE x a b
  embed (SeqEB a b)   = SeqE a b
  embed FailEB        = FailE
  embed FallThroughEB = FallThroughE

sub :: (Id -> Maybe Expr) -> Expr -> Expr
sub tr = cata $ \e ->
  case e of
    FreeEB i -> fromMaybe (FreeE i) (tr i)
    other   -> embed other

alphaEq :: Expr -> Expr -> Bool
alphaEq = alphaEq' (H.empty, H.empty, 0)

alphaEq' :: (H.Map Id Int, H.Map Id Int, Int) -> Expr -> Expr -> Bool
alphaEq' lvlInfo@(o', p', _) = eq
  where
    eq (FnE [] e)     (FnE [] f)     = e `eq` f
    eq (FnE (x:xs) e) (FnE (y:ys) f) = alphaEq' (pushVars (x, y) lvlInfo) (FnE xs e) (FnE ys f)
    eq (LetE x e f)   (LetE y g h)   = e `eq` g && alphaEq' (pushVars (x, y) lvlInfo) f h
    eq (FreeE x)      (FreeE y)      = fromMaybe (x == y)
                                     $ (==) <$> checkL x <*> (checkR y <|> Just (-1))
    eq (CaseE e as)   (CaseE f bs)   = e `eq` f
                                    && length as == length bs
                                    && all (uncurry armsEq) (zip as bs)
    eq e              f              = (structEq `on` project) e f

    structEq e f = e `shapeEq` f && and ((zipWith eq `on` toList) e f)

    armsEq (q, e) (r, f) = (isVar q && isVar r || q `shapeEq` r)
                        && let vars = zip (patVars q) (patVars r) in
                             alphaEq' (foldr pushVars lvlInfo vars) e f

    pushVars (x, y) (o, p, lvl) = (H.insert x lvl o, H.insert y lvl p, lvl + 1)

    checkL x = H.lookup x o'
    checkR y = H.lookup y p'
