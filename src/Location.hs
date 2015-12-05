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
                   } deriving (Eq, Ord)

-- | A representation of a location in the source file, as a line and column
-- (for errors) as well as an offset and width, for slicing from the input
-- stream.
data Span      = S { start  :: !Point
                   , offset :: !Int
                   , width  :: !Int
                   }
               | Floating
                 deriving Eq

instance Show Point where
  show (P l c) = show l ++ ":" ++ show c

instance Show Span where
  show Floating  = "~~~"
  show (S s o w) = "[" ++ show s ++ "]" ++ "+" ++ show o ++ "~" ++ show w

fmtErr :: Show a => Located a -> String
fmtErr (L (S (P l c) _ _) x) =
  concat [ show x
         , " at line ", show l
         , ", column ", show c
         ]

fmtErr (L Floating x) = show x

-- | A commutative monoidal operation on spans. Combining any span with the
-- `Floating` span will leave it unchanged. Otherwise, combining two spans
-- creates the smallest span that contains them both.
instance Monoid Span where
  mempty = Floating

  mappend s Floating = s
  mappend Floating s = s

  mappend (S p m v) (S q o w) =
    S (p `min` q) off (end - off)
    where
      off = m `min` o
      end = (m + v) `max` (o + w)

-- | This instance describes how to combine values annotated with a location
-- whilst preserving the consistency of that location information.
instance Applicative Located where
  pure                = L Floating
  (L s f) <*> (L t x) = L (s <> t) (f x)

-- | Given a structure containing located things, produce a located structure,
-- by combining the internal location annotations according to the above
-- Applicative instance.
loc :: Traversable f => f (Located a) -> Located (f a)
loc = sequenceA

-- | Relieve an annotated value of its location annotation.
dislocate :: Located a -> a
dislocate (L _ a) = a
