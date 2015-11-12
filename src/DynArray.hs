{-# LANGUAGE NamedFieldPuns #-}

{-|
Mutable, resizable STArrays with integer indices.
Interface includes both random access and a stack interface.
|-}
module DynArray ( DynArray
                , newArray_
                , fetch
                , peek
                , pop'
                , pop
                , push
                ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.ST as STA
import Data.STRef

data DArray s e = DArray { buf  :: STA.STArray s Int e
                         , size :: !Int
                         , cap  :: !Int
                         }

newtype DynArray s e = DA (STRef s (DArray s e))

-- | Create a new Dynamic Array.
newArray_ :: Int
          -- ^ Initial Capacity
          -> ST s (DynArray s e)

newArray_ c = do
  arr <- STA.newArray_ (0, c - 1)
  ref <- newSTRef $ DArray { buf = arr, size = 0, cap = c }
  return $ DA ref

-- | Array Random Access
fetch :: DynArray s e -> Int -> ST s e
fetch (DA ref) i = do
  arr <- readSTRef ref
  STA.readArray (buf arr) i

-- | Access the last element in the array.
peek :: Int -> DynArray s e -> ST s e
peek off (DA ref) = do
  DArray {buf, size} <- readSTRef ref
  STA.readArray buf (size - off)

-- | Remove the last element in the array. This could cause the array to become
-- compacted.
pop :: DynArray s e -> ST s ()
pop (DA ref) = do
  modifySTRef ref decSize
  compact ref
  where
    decSize d@DArray{size = s} = d{size = 0 `max` s-1}

-- | Variant of @ pop @ that returns the last element as well as removing it.
pop' :: DynArray s e -> ST s e
pop' d = do { x <- peek 1 d; pop d; return x }

-- | Append an element to the end of the array. This could trigger an array
-- expansion.
push :: DynArray s e -> e -> ST s ()
push (DA ref) x = do
  makeRoom ref
  DArray {buf, size} <- readSTRef ref
  STA.writeArray buf size x
  modifySTRef ref incSize
  where
    incSize d@DArray{size = s} = d{size = s+1}

compact, makeRoom :: STRef s (DArray s e) -> ST s ()

compact ref = do
  DArray {buf, size, cap} <- readSTRef ref
  when (size < cap `div` 4) $ do
    let cap' = cap `div` 2
    buf' <- resize buf cap'
    writeSTRef ref $ DArray {buf = buf', cap = cap', size}

makeRoom ref = do
  DArray {buf, size, cap} <- readSTRef ref
  when (size >= cap) $ do
    let cap' = 2 * cap
    buf' <- resize buf cap'
    writeSTRef ref $ DArray {buf = buf', cap = cap', size}

resize :: STA.STArray s Int e -> Int -> ST s (STA.STArray s Int e)
resize from cap = STA.getElems from >>= STA.newListArray (0, cap - 1)
