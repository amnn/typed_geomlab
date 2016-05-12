{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Generalisation of types to form type schemes, and instantiation (vice versa).

|-}
module Infer.Generalise where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.ST.Class
import           Control.Monad.State
import           Data.HashMap.Strict    as H
import           Data.Monad.State
import           Data.Monad.Type
import           Infer.Levels
import           Infer.Monad
import           Infer.TypeFactory

-- | Find all type references from lower scopes than the current one, and ensure
-- that level adjustments have all been made for them. This is done before a
-- generalisation, as a level adjustment could result in a type not being
-- generalised.
forceDelayedAdjustments :: MonadInfer m => m ()
forceDelayedAdjustments = do
  SS {gsRef}             <- ask
  gs@GS{waitingToAdjust} <- readIRef gsRef
  delayed                <- foldM adjustTop [] waitingToAdjust
  writeIRef gsRef gs{waitingToAdjust = delayed}
  where
    adjustTop ts tr = do
      lvl <- Lvl <$> asks lvl
      Ty {subs, newLevel = Set nLvl, oldLevel = oLvl} <- readIRef tr
      case () of
        _ | oLvl <= lvl  -> return (tr:ts)
        _ | oLvl == nLvl -> return ts
        _ -> do
          Set mLvl <- markTy 0 tr
          ts'  <- foldM (adjustRec nLvl) ts . allChildren $ subs
          unifyLevels tr mLvl
          return ts'

    adjustRec nLvl ts _tr = do
      _tr <- repr _tr
      Ty {newLevel = mNLvl'} <- readIRef _tr
      case mNLvl' of
        Marked _  -> adjustTop ts _tr
        Set nLvl' -> do
          when (nLvl' > nLvl) (setLevel _tr $ Set nLvl)
          adjustTop ts _tr

-- | Generalisation involves universally quantifying variables that are scoped
-- strictly below the current level.
generalise :: MonadInfer m => TyRef (World m) -> m ()
generalise tr = do
  forceDelayedAdjustments
  gen tr
  where
    gen _ur = do
      _ur <- repr _ur
      lvl <- Lvl <$> asks lvl
      Ty {subs, newLevel} <- readIRef _ur
      case newLevel of
        Marked _ -> return ()
        Set l    -> when (l > lvl) $ do
          afterMarking 0 _ur $ mapM_ gen (allChildren subs)
          unifyLevels _ur $ Gen

-- | Replace universally quantified variables in a (possibly) general type with
-- fresh type variables at the current level: Every instance of a generalised
-- type is new, and unification with one instance should not affect another.
instantiate :: MonadInfer m => TyRef (World m) -> m (TyRef (World m))
instantiate tRef = evalStateT (inst tRef) H.empty
  where
    inst tr = do
      Ty {uid, subs, newLevel = Set lvl} <- readIRef =<< repr tr
      subst <- get
      case H.lookup uid subst of
        Just tr' -> return tr'
        Nothing | Gen <- lvl -> do
          nr <- newTy H.empty
          put (H.insert uid nr subst)
          nSubs <- mapM instSub subs
          modifyIRef nr $ \t -> t {subs = nSubs}
          return nr
        _ -> return tr

    instSub s@Sub {children} = do
      cs' <- mapM inst children
      return s { children = cs' }
