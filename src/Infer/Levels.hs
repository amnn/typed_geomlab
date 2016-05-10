{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

{-|

Functions for working with types annotated with levels.

|-}
module Infer.Levels where

import Infer.Monad
import Data.Monad.Type
import Control.Monad.ST.Class
import Data.Monad.State
import Control.Monad.Reader

-- | Add a type reference to the global list of references waiting to have their
-- levels adjusted.
delayLevelUpdate :: MonadInfer m => TyRef (World m) -> m ()
delayLevelUpdate tr = do
  SS {gsRef} <- ask
  modifyIRef gsRef (delayTy tr)
  where
    delayTy t is@GS {waitingToAdjust = wta} = is {waitingToAdjust = t : wta}

-- | Mark a reference as having been visited. Doing so overwrites its level
-- value, so that is returned so that it may be restored later. If a marked
-- reference is visited more than once, this indicates a cycle has been
-- detected.
markTy :: MonadInfer m => Int -> TyRef (World m) -> m (Marked Level)
markTy i tr = do
  sty <- readIRef tr
  writeIRef tr $ sty{newLevel = Marked i}
  return $ newLevel sty

-- | Set the level of a type reference.
setLevel :: MonadInfer m => TyRef (World m) -> Marked Level -> m ()
setLevel tr mLvl = do
  modifyIRef tr $ \st -> st{newLevel = mLvl}

-- | Mark a reference, then perform an action and immediately unmark said
-- reference (Well scoped marking during traversals).
afterMarking :: MonadInfer m => Int -> TyRef (World m) -> m a ->  m a
afterMarking i tr act = do
  mLvl <- markTy i tr
  ret  <- act
  setLevel tr mLvl
  return ret

-- | Get the level of a type reference.
getLevel :: MonadInfer m => TyRef (World m) -> m Level
getLevel tr = do
  StratTy{newLevel = Set lvl} <- readIRef tr
  return lvl

-- | Set all level values of a type as the same. When this is done to non-leaf
-- types, it indicates that all child levels have been properly adjusted and
-- accounted for.
unifyLevels :: MonadInfer m => TyRef (World m) -> Level -> m ()
unifyLevels tr lvl =
  modifyIRef tr $ \st -> st{newLevel = Set lvl, oldLevel = lvl}
