{-# LANGUAGE DeriveGeneric #-}

{-|

Representation of types used within the Infer Monad, as in-memory
graphs. Variables can be either identifiers, or a forward pointer to another
type, and each variable is also annotated with levels, used for generalisation.

|-}
module Data.Monad.Type where

import           Control.Monad       ((>=>))
import           Data.Constructor
import           Data.Flag
import qualified Data.HashMap.Strict as H
import           Data.STRef

type TyRef s = STRef s (Ty s)

data FlagTree s = FT { caseArg :: TyRef s
                     , arms    :: !(H.HashMap Ctr (FlagTree s))
                     , interp  :: !Flag
                     }
                | FL { interp  :: !Flag }
                  deriving Eq

-- | An individual sub-type in the Remy encoding
data Sub s = Sub { flag     :: !(FlagTree s)
                 , children :: [TyRef s]
                 }
             deriving Eq

-- | A representation of the level of a type, with a notion of ordering. @ Gen @
-- represents the "generalised" level, which is considered higher than all other
-- levels.
data Level = Lvl Int | Gen deriving (Eq, Show, Ord)

-- | Tag type for cycle detection (a's are marked as they are visited, if a
-- marked object is visited again, then we have detected a cycle).
data Marked a = Set a | Marked !Int deriving (Eq, Show)

-- | Remy encoding of types
data Ty s = Ty  { uid      :: !Int
                , subs     :: !(Maybe (H.HashMap Ctr (Sub s)))
                , deps     :: ![(TyRef s, Ctr)]
                , newLevel :: !(Marked Level)
                , oldLevel :: !Level
                }
          | Fwd (TyRef s)
            deriving Eq

allChildren :: Maybe (H.HashMap Ctr (Sub s)) -> [TyRef s]
allChildren = maybe [] (H.elems >=> children)
