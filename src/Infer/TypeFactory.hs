{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Ways to build types as represented internally by the type checker.

|-}
module Infer.TypeFactory where

import           Control.Monad.Reader
import           Control.Monad.ST.Class
import           Data.Char              (chr, ord)
import           Data.Monad.DynArray
import           Data.Monad.State
import           Data.Monad.Type
import           Data.Token             (Id)
import           Data.Type
import           Infer.Monad

-- | Introduce a new local variable to the stack, and return a reference to it.
pushLocal :: MonadInfer m => m (TyRef (World m))
pushLocal = do
  tyCtx <- getTyCtx
  tr    <- newVar
  liftST $ push tyCtx tr
  return tr

-- | Remove the top-most local variable from the stack.
popLocal :: MonadInfer m => m ()
popLocal = getTyCtx >>= liftST . pop

-- | Produce a unique variable name.
fresh :: MonadInfer m => m Id
fresh = do
  SS {gsRef}     <- ask
  GS {nextTyVar} <- readIRef gsRef
  modifyIRef gsRef bumpVar
  return (toId nextTyVar)
  where
    bumpVar is@GS {nextTyVar = n} = is {nextTyVar = n + 1}

    a = ord 'a'
    toId x
      | x < 26    = [chr (a + x)]
      | otherwise = 't' : show (x - 26)

-- | Create a new type at the current level, from the structure provided.
newTy :: MonadInfer m
      => TyB (StratV (World m)) (TyRef (World m))
      -- ^ The structure of the type
      -> m (TyRef (World m))
      -- ^ A reference to a type with the given structure, created at the
      -- current level.

newTy t = do
  lvl <- getCurrLvl
  newIRef $ StratTy { ty = t, newLevel = Set (Lvl lvl), oldLevel = Lvl lvl }

-- | Create a new type at the generic level, from the structure provided.
genTy :: MonadST m => TyB (StratV (World m)) (TyRef (World m)) -> m (TyRef (World m))
genTy t = newIRef $ StratTy { ty = t, newLevel = Set Gen, oldLevel = Gen }

-- | Create a fresh type variable at the current level.
newVar :: MonadInfer m => m (TyRef (World m))
newVar = fresh >>= newTy . VarTB . FreeV
