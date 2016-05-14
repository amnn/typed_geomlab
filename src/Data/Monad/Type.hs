{-# LANGUAGE DeriveGeneric #-}

{-|

Representation of types used within the Infer Monad, as in-memory
graphs. Variables can be either identifiers, or a forward pointer to another
type, and each variable is also annotated with levels, used for generalisation.

|-}
module Data.Monad.Type where

import           Control.Monad       ((>=>))
import           Data.Flag
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           Data.STRef
import           Data.Token          (Id)
import           GHC.Generics        (Generic)

type TyRef s = STRef s (Ty s)

-- | Constructors
data Ctr = Bool | Num | Str | Nil | Cons | Fn !Int
         | Tag Id | Atom
         -- ^ Ordering is important, `Atom` appears after `Tag` so that the
         -- latter is unified first.
         | Any
         -- ^ Similarly, `Any` must be unified last of all.
           deriving (Eq, Ord, Generic)

data Sub s = Sub { flag     :: !Flag
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
                , subs     :: Maybe (H.HashMap Ctr (Sub s))
                , newLevel :: !(Marked Level)
                , oldLevel :: !Level
                }
          | Fwd (TyRef s)
            deriving Eq

instance Hashable Ctr

instance Show Ctr where
  show  Bool   = "bool"
  show  Num    = "num"
  show  Str    = "str"
  show  Nil    = "[]"
  show  Cons   = "(:)"
  show (Fn a)  = "(" ++ show a ++ ")->"
  show (Tag t) = "#" ++ t
  show  Atom   = "atom"
  show  Any    = "any"

-- | Mapping from a constructor to its wildcard constructor.
wildcard :: Ctr -> Maybe Ctr
wildcard  Any    = Nothing
wildcard (Tag _) = Just Atom
wildcard  _      = Just Any

-- | How many child types each constructor requires
arity :: Ctr -> Int
arity  Cons    = 2
arity (Fn aty) = aty + 1
arity  _       = 0

allChildren :: Maybe (H.HashMap Ctr (Sub s)) -> [TyRef s]
allChildren = maybe [] (H.elems >=> children)
