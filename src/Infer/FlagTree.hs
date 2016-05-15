{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

{-|

Flag Tree operations.

|-}
module Infer.FlagTree where

import           Control.Monad          (foldM)
import           Control.Monad.ST.Class
import           Data.Constructor
import           Data.Flag
import qualified Data.HashMap.Strict    as H
import           Data.Monad.Type
import qualified Data.Set               as S
import           Infer.Monad

-- | Join together the labels of flag trees in the collection.
joinLabels :: (Foldable f, Functor f) => f (FlagTree s) -> Flag
joinLabels arms = foldl1 (\/) (interp <$> arms)

-- | Return the set of constructors that label edges out of nodes labelled by
-- the given type reference in the given flag tree.
cases :: MonadInferTop m
      => TyRef (World m)
      -> FlagTree (World m)
      -> m (S.Set Ctr)
cases _   FL {}              = return S.empty
cases _tr FT {caseArg, arms} = do
  _cr <- repr caseArg
  if _cr == _tr then
    return . S.fromList . H.keys $ arms
  else
    foldl1 S.union <$> traverse (cases _tr) arms

{- Definitions of operations in the project follow -}

specialise :: MonadInferTop m
           => H.HashMap Int Ctr
           -> FlagTree (World m)
           -> m (FlagTree (World m))

specialise _   flg@FL {} = return flg
specialise ctx flg@FT {caseArg, arms, interp} = do
  arms'     <- traverse (specialise ctx) arms
  Ty {uid}  <- readIRef =<< repr caseArg
  case H.lookup uid ctx of
    Nothing  -> return flg {arms = arms'}
    Just ctr
      | Just f' <- H.lookup ctr arms' -> return  f'
      | otherwise                     -> return (FL interp)

merge :: MonadInferTop m
      =>    FlagTree (World m)
      ->    FlagTree (World m)
      -> m (FlagTree (World m))

merge   (FL i) (FL j) = return (FL (i /\ j))
merge f@(FL _)  g     = merge g f

merge f@FT {caseArg, arms, interp} g = do
  _cr        <- repr caseArg
  Ty {uid}   <- readIRef _cr
  gCases     <- cases _cr g
  arms'      <- mkMap (fCases `S.union` gCases) (newArm uid)
  return $ f { arms = arms' }
  where
    fCases        = S.fromList (H.keys arms)
    lookupArm ctr = H.lookupDefault (FL interp) ctr arms
    mkMap ks val  = H.fromList <$> mapM (\k -> (k,) <$> val k) (S.toList ks)

    newArm uid ctr = do
      gArm <- specialise (H.singleton uid ctr) g
      merge (lookupArm ctr) gArm

decorrelate :: MonadInferTop m
            =>    TyRef    (World m)
            ->    FlagTree (World m)
            -> m (FlagTree (World m))

decorrelate _   f@FL {} = return f

decorrelate _tr f@FT {caseArg, arms} = do
  arms' <- traverse (decorrelate _tr) arms
  _cr   <- repr caseArg
  if _tr == _cr then
    foldM merge (FL dontCare) arms'
  else
    return f { arms = arms' }
