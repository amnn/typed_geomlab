module Data.Structure where

import           Data.Function (on)

-- | Isolate the shape of a functor from the data it holds.
shape :: Functor f => f a -> f ()
shape = fmap (const ())

shapeEq :: (Functor f, Eq (f ())) => f a -> f a -> Bool
shapeEq = (==) `on` shape
