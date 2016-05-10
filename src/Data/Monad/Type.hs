{-|

Representation of types used within the Infer Monad, as in-memory
graphs. Variables can be either identifiers, or a forward pointer to another
type, and each variable is also annotated with levels, used for generalisation.

|-}
module Data.Monad.Type where

import           Data.STRef
import           Data.Token (Id)
import           Data.Type

type TyRef s = STRef s (StratTy s)

-- | A representation of the level of a type, with a notion of ordering. @ Gen @
-- represents the "generalised" level, which is considered higher than all other
-- levels.
data Level = Lvl Int | Gen deriving (Eq, Show, Ord)

-- | Representation of variables. Each can either be a name, or a link to
-- another type reference.
data StratV s = FreeV Id | FwdV (TyRef s) deriving Eq

-- | Tag type for cycle detection (a's are marked as they are visited, if a
-- marked object is visited again, then we have detected a cycle).
data Marked a = Set a | Marked !Int deriving (Eq, Show)

-- | Representation of types, annotated by their level.
data StratTy s = StratTy { ty       :: TyB (StratV s) (TyRef s)
                         , newLevel :: !(Marked Level)
                         , oldLevel :: !Level
                         } deriving Eq
