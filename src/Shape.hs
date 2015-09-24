module Shape where

import Token(Id)

data Shape a = NumS Double
             | StrS String
             | NilS
             | AtomS Id
             | ConsS a a
               deriving (Eq, Show)

class HasShape a where
  embedShape :: Shape a -> a

numS :: HasShape a => Double -> a
numS = embedShape . NumS

strS :: HasShape a => String -> a
strS = embedShape . StrS

nilS :: HasShape a => a
nilS = embedShape NilS

atomS :: HasShape a => Id -> a
atomS = embedShape . AtomS

consS :: HasShape a => a -> a -> a
consS x xs = embedShape (ConsS x xs)

enlist, enlist1 :: HasShape a => [a] -> a
enlist  = foldl  (flip consS) nilS
enlist1 = foldl1 (flip consS)
