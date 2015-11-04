{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Literal where

import Token (Id)

-- | Structure of Literal values shared by Patterns, the AST and the Desugared
-- AST.
data LitB a = NumB Double
            | StrB String
            | NilB
            | AtomB Id
            | ConsB a a
              deriving ( Eq
                       , Ord
                       , Show
                       , Functor
                       , Foldable
                       , Traversable
                       )

-- | Any data structure which can internalise the shape of a literal should
-- implement this class, in order to make the creation of literals in that
-- structure easier.
class EmbedsLit a where
  embedLit :: LitB a -> a

numB :: EmbedsLit a => Double -> a
numB = embedLit . NumB

strB :: EmbedsLit a => String -> a
strB = embedLit . StrB

nilB :: EmbedsLit a => a
nilB = embedLit NilB

atomB :: EmbedsLit a => Id -> a
atomB = embedLit . AtomB

consB :: EmbedsLit a => a -> a -> a
consB x xs = embedLit (ConsB x xs)

enlist, enlist1 :: EmbedsLit a => [a] -> a
enlist  = foldl  (flip consB) nilB
enlist1 = foldl1 (flip consB)
