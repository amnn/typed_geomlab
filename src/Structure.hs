module Structure where

-- | Isolate the shape of a functor from the data it holds.
shape :: Functor f => f a -> f ()
shape = fmap (const ())
