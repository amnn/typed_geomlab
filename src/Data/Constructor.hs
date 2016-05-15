{-# LANGUAGE DeriveGeneric #-}

{-|

Type Constructors.

|-}
module Data.Constructor where

import           Data.Hashable
import           Data.Token    (Id)
import           GHC.Generics  (Generic)

-- | Constructors
data Ctr = Bool | Num | Str | Nil | Cons | Fn !Int
         | Tag Id | Atom
         -- ^ Ordering is important, `Atom` appears after `Tag` so that the
         -- latter is unified first.
         | Any
         -- ^ Similarly, `Any` must be unified last of all.
           deriving (Eq, Ord, Generic)

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
