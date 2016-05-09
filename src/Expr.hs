{-# LANGUAGE TypeFamilies, DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Expr where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Functor.Foldable
import Literal
import Location
import Patt
import Token (Id)

-- | The AST, after it has been desugared. Changes include:
--
--    * de Bruijn index representation of local variables (Free variables retain
--    their string identifier).
--
--    * Pattern matching is de-coupled from the function notation, into a
--    separate Case expression which does not support nesting. Functions that
--    use patterns are desugared into a function whose formal parameters are
--    passed directly to a case expression.
--
--    * Function application supports arbitrary expressions in the callable
--    position, not just identifiers.
--
--    * Extra expressions (@ FailE @, @ FallThroughE @) have been added to
--    support the proper compilation of pattern matching.
data Expr = LitE (LitB Expr)
          | VarE !Int
          | FreeE Id
          | IfE Expr Expr Expr
          | CaseE Expr [(SimplePatt, Expr)]
          | FnE !Int Expr
          | AppE Expr [Expr]
          | LetE Expr Expr
          | LocE String (Located Expr)

          -- Case Expression Specific
          | FailE
          | FallThroughE
            deriving (Eq, Show)

-- | A functor whose least-fixed point is isomorphic to @ Expr @.
data ExprB a = LitEB (LitB a)
             | VarEB !Int
             | FreeEB Id
             | IfEB a a a
             | CaseEB a [(SimplePatt, a)]
             | FnEB !Int a
             | AppEB a [a]
             | LetEB a a
             | LocEB String (Located a)
             | FailEB
             | FallThroughEB
               deriving ( Eq, Show
                        , P.Foldable
                        , Traversable
                        , Functor)

instance EmbedsLit Expr where
  embedLit = LitE

-- | Remove location annotations from AST
stripLoc :: Expr -> Expr
stripLoc = cata s
  where
    s (LocEB _ le) = dislocate le
    s e            = embed e

type instance Base Expr = ExprB
instance Foldable Expr where
  project (LitE s)      = LitEB s
  project (VarE x)      = VarEB x
  project (FreeE x)     = FreeEB x
  project (IfE c t e)   = IfEB c t e
  project (CaseE e as)  = CaseEB e as
  project (FnE n e)     = FnEB n e
  project (AppE f xs)   = AppEB f xs
  project (LetE a b)    = LetEB a b
  project (LocE lbl le) = LocEB lbl le
  project FailE         = FailEB
  project FallThroughE  = FallThroughEB

instance Unfoldable Expr where
  embed (LitEB s)      = LitE s
  embed (VarEB x)      = VarE x
  embed (FreeEB x)     = FreeE x
  embed (IfEB c t e)   = IfE c t e
  embed (CaseEB e as)  = CaseE e as
  embed (FnEB n e)     = FnE n e
  embed (AppEB f xs)   = AppE f xs
  embed (LetEB a b)    = LetE a b
  embed (LocEB lbl le) = LocE lbl le
  embed FailEB         = FailE
  embed FallThroughEB  = FallThroughE
