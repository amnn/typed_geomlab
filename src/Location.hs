{-# LANGUAGE DeriveFunctor #-}
module Location where

-- | An annotation linking the (parameterised) data to a specific location in
-- the source.
data Located a = L Span a deriving (Eq, Functor)

-- | A line and column, used for printing error messages.
data Point     = P { line   :: !Int
                   , col    :: !Int
                   } deriving (Eq, Show)

-- | A representation of a location in the source file, as a line and column
-- (for errors) as well as an offset and width, for slicing from the input
-- stream.
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
