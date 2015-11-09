{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Patt where

import Prelude hiding (Foldable)
import qualified Prelude as P (Foldable)
import Data.Foldable (toList)
import Data.Functor.Foldable
import Literal
import Structure
import Token (Id)

-- | Structure of patterns used in the formal parameters of functions in the @
-- Sugar @ AST and in Case expressions in the @ Expr @ AST.
data Patt = ValP (LitB Patt)
          | VarP Id
            deriving (Eq, Show)

-- | A functor whose least-fixed point is isomorphic to @ Patt @.
data PattB a = ValPB (LitB a)
             | VarPB Id
               deriving ( Eq
                        , Ord
                        , Show
                        , P.Foldable
                        , Functor
                        , Traversable
                        )

-- | Individual generator in a list comprehension.
data GenB a = GenB Patt a
              -- ^ A generator that consumes a list. @ GenB p e @ corresponds to
              -- @ ..., p <- e, ... @ in the source.
            | FilterB a
              -- ^ A generator that filters previous values. @ FilterB e @
              -- corresponds to @ ..., when e, ... @ in the source.
              deriving (Eq, Show, Functor)

-- | A single arm of a function definition in the source AST, storing the name
-- of the function, the fully nested pattern to match against, the function body
-- and an optional boolean guard.
data FnArmB a = FnArm Id [Patt] a (Maybe a)
                deriving ( Eq
                         , Show
                         , Functor
                         , P.Foldable
                         , Traversable
                         )

-- | A non-nested pattern. Sub-patterns are replaced with "holes" that represent
-- variables that this pattern introduces.
type SimplePatt = PattB ()

type instance Base Patt = PattB
instance Foldable Patt where
  project (ValP s)      = ValPB s
  project (VarP x)      = VarPB x

instance EmbedsLit Patt where
  embedLit = ValP


isVar :: PattB a -> Bool
isVar (VarPB _) = True
isVar _         = False

-- | Count the number of variables that will be introduced by a pattern if it is
-- simplified.
holes :: PattB a -> Int
holes (VarPB _) = 1
holes p         = length (toList p)

-- | Skim the top most structure from a pattern.
simplify :: Patt -> SimplePatt
simplify = shape . project

-- | Discard the top-most pattern structure and return the underlying patterns.
subPats :: Patt -> [Patt]
subPats = toList . project
