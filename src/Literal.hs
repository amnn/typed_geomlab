{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Literal where

import Token (Id)

data LitB a = NumB Double
            | StrB String
            | NilB
            | AtomB Id
            | ConsB a a
              deriving ( Eq
                       , Show
                       , Functor
                       , Foldable
                       , Traversable
                       )

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
