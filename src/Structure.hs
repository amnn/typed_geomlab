module Structure where

shape :: Functor f => f a -> f ()
shape = fmap (const ())

shapeEq :: (Functor f, Eq (f ())) => f a -> f b -> Bool
shapeEq a b = shape a == shape b
