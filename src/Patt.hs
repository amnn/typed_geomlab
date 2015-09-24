module Patt where

import Token (Id)
import Shape

data Patt = ValP (Shape Patt)
          | AnonP
          | VarP Id
          | OffsetP Patt Double
            deriving (Eq, Show)

instance HasShape Patt where
  embedShape = ValP
