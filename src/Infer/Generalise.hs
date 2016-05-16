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
import Data.Constructor

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


-- | A single entry in the instantiation lookaside map. It either contains a
-- complete, instantiated copy of a previously general type, or a list of types
-- that will depend upon the instantiation.
data InstRecord s = Done (TyRef s) | Waiting (TyRef s) [(TyRef s, Ctr)]

-- | Replace universally quantified variables in a (possibly) general type with
-- fresh type variables at the current level: Every instance of a generalised
-- type is new, and unification with one instance should not affect another.
instantiate :: MonadInfer m => TyRef (World m) -> m (TyRef (World m))
instantiate tRef = evalStateT (inst tRef) H.empty
  where
    inst tr = do
      Ty {uid, subs, newLevel} <- readIRef =<< repr tr
      lookaside <- get
      case H.lookup uid lookaside of
        Just (Done tr')      -> return tr'
        Just (Waiting fwd _) -> do
          nr <- newInst uid subs
          writeIRef fwd (Fwd nr)
          return nr

        Nothing | Set Gen  <- newLevel -> newInst uid subs
                | Marked _ <- newLevel -> error "instantiate: Marked level!"

        _ -> return tr

    newInst uid subs = do
      nr        <- newTy (Just H.empty)
      lookaside <- get
      put (H.insert uid (Done nr) lookaside)
      subs' <- mapM (H.traverseWithKey (instSub nr)) subs
      modifyIRef nr $ \t -> t {subs = subs'}
      return nr

    instSub nr ctr s@Sub {flag, children} = do
      f'  <- instFlag nr ctr flag
      cs' <- mapM inst children
      return s { flag = f', children = cs' }

    instFlag _  _   f@FL {}              = return f
    instFlag nr ctr f@FT {caseArg, arms} = do
      arms' <- mapM (instFlag nr ctr) arms
      cr'   <- registerDependantInst caseArg nr ctr
      return f { caseArg = cr', arms = arms' }

    registerDependantInst _cr nr ctr = do
      _cr                <- repr _cr
      Ty {uid, newLevel} <- readIRef _cr
      case newLevel of
        Marked _    -> error "instantiate: Marked level!"
        Set (Lvl _) -> return _cr
        Set  Gen    -> do
          lookaside <- get
          case H.lookup uid lookaside of
            Just (Done cr')         -> return cr'
            Just (Waiting fwd wait) -> do
              modify (H.insert uid (Waiting fwd ((nr, ctr):wait)))
              return fwd

            Nothing -> do
              fwd <- newVar
              modify (H.insert uid (Waiting fwd [(nr, ctr)]))
              return fwd
