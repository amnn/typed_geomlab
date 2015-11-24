{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Location where

import Data.Monoid ((<>))

-- | An annotation linking the (parameterised) data to a specific location in
-- the source.
data Located a = L Span a
                 deriving ( Eq
                          , Show
                          , Foldable
                          , Functor
                          , Traversable
                          )

-- | A line and column, used for printing error messages.
data Point     = P { line   :: !Int
                   , col    :: !Int
                   } deriving (Eq, Show, Ord)

-- | A representation of a location in the source file, as a line and column
-- (for errors) as well as an offset and width, for slicing from the input
-- stream.
data Span      = S { start  :: !Point
                   , offset :: !Int
                   , width  :: !Int
                   }
               | Floating
                 deriving (Eq, Show)

fmtErr :: Show a => Located a -> String
fmtErr (L (S (P l c) _ _) x) =
  concat [ show x
         , " at line ", show l
         , ", column ", show c
         ]

fmtErr (L Floating x) = show x

instance Monoid Span where
  mempty = Floating

  mappend s Floating = s
  mappend Floating s = s

  mappend (S p m v) (S q o w) =
    S (p `min` q) off (end - off)
    where
      off = m `min` o
      end = (m + v) `max` (o + w)

instance Applicative Located where
  pure                = L Floating
  (L s f) <*> (L t x) = L (s <> t) (f x)

loc :: Traversable f => f (Located a) -> Located (f a)
loc = sequenceA

dislocate :: Located a -> a
dislocate (L _ a) = a
