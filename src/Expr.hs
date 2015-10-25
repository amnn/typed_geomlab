{-# LANGUAGE TypeFamilies, DeriveFunctor #-}

module Expr where

import Prelude hiding (Foldable)
import Control.Applicative ((<|>))
import Data.Function (on)
import Data.Functor.Foldable
import qualified Data.HashMap as H
import Data.Maybe (fromMaybe)
import Literal
import Patt
import Token (Id)

data Expr = LitE (LitB Expr)
          | VarE Id
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
             | VarEB Id
             | IfEB a a a
             | CaseEB a [(SimplePatt, a)]
             | FnEB [Id] a
             | AppEB a [a]
             | LetEB Id a a
             | SeqEB a a
             | FailEB
             | FallThroughEB
               deriving (Eq, Show, Functor)

instance EmbedsLit Expr where
  embedLit = LitE

type instance Base Expr = ExprB
instance Foldable Expr where
  project (LitE s)     = LitEB s
  project (VarE x)     = VarEB x
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
    VarEB i -> fromMaybe (VarE i) (tr i)
    other   -> embed other

alphaEq :: Expr -> Expr -> Bool
alphaEq = alphaEq' (H.empty, H.empty, 0)

alphaEq' :: (H.Map Id Int, H.Map Id Int, Int) -> Expr -> Expr -> Bool
alphaEq' lvlInfo = eq
  where
    eq (LitE (ConsB x y)) (LitE (ConsB z w)) = x `eq` z && y `eq` w
    eq (LitE l)           (LitE m)           = l == m

    eq (IfE c t e)    (IfE d u f)    = c `eq` d && t `eq` u && e `eq` f
    eq (SeqE a b)     (SeqE c d)     = a `eq` c && b `eq` d
    eq (AppE f xs)    (AppE g ys)    = and (zipWith eq (f:xs) (g:ys))

    eq (CaseE e as)   (CaseE f bs)   = e `eq` f
                                    && length as == length bs
                                    && all (uncurry armsEq) (zip as bs)

    eq (FnE [] e)     (FnE [] f)     = e `eq` f
    eq (FnE (x:xs) e) (FnE (y:ys) f) = alphaEq' (pushVars (x, y) lvlInfo) (FnE xs e) (FnE ys f)

    eq (LetE x e f)   (LetE y g h)   = e `eq` g && alphaEq' (pushVars (x, y) lvlInfo) f h

    eq (VarE x)       (VarE y)       = fromMaybe (x == y)
                                     $ (==) <$> checkL x <*> (checkR y <|> Just (-1))

    eq e f = e == f

    armsEq (q, e) (r, f) = (isVar q && isVar r || q `shapeEq` r)
                        && let vars = zip (patVars q) (patVars r) in
                             alphaEq' (foldr pushVars lvlInfo vars) e f

    shapeEq = (==) `on` patShape

    pushVars (x, y) (o, p, lvl) = (H.insert x lvl o, H.insert y lvl p, lvl + 1)

    checkL x = let (o, _, _) = lvlInfo in H.lookup x o
    checkR y = let (_, p, _) = lvlInfo in H.lookup y p
