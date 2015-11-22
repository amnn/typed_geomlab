{-# LANGUAGE DeriveFunctor #-}
module Location where

data Located a = L Span a deriving (Eq, Functor)

data Point     = P { line   :: !Int
                   , col    :: !Int
                   } deriving (Eq, Show)

data Span      = S { start  :: !Point
                   , offset :: !Int
                   , width  :: !Int
                   } deriving (Eq, Show)

instance Show a => Show (Located a) where
  show (L (S (P l c) _ _) a) =
    concat [ show a
           , " at line ", show l
           , ", column ", show c
           ]

emptySpan :: Span
emptySpan = S (P 0 0) 0 0
