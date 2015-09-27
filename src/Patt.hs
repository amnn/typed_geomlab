module Patt where

import Literal
import Token (Id)

data Patt = ValP (LitShape Patt)
          | AnonP
          | VarP Id
          | OffsetP Patt Double
            deriving (Eq, Show)

instance EmbedsLit Patt where
  embedLit = ValP
