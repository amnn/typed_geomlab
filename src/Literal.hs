{-# LANGUAGE DeriveFunctor #-}

module Literal where

import Token (Id)

data LitShape a = NumS Double
                | StrS String
                | NilS
                | AtomS Id
                | ConsS a a
                  deriving (Eq, Show, Functor)

class EmbedsLit a where
  embedLit :: LitShape a -> a

numS :: EmbedsLit a => Double -> a
numS = embedLit . NumS

strS :: EmbedsLit a => String -> a
strS = embedLit . StrS

nilS :: EmbedsLit a => a
nilS = embedLit NilS

atomS :: EmbedsLit a => Id -> a
atomS = embedLit . AtomS

consS :: EmbedsLit a => a -> a -> a
consS x xs = embedLit (ConsS x xs)

enlist, enlist1 :: EmbedsLit a => [a] -> a
enlist  = foldl  (flip consS) nilS
enlist1 = foldl1 (flip consS)
