{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Type Unification

|-}
module Infer.Unify where

import           Control.Monad.Except
import           Control.Monad.ST.Class
import           Data.Foldable          (toList)
import           Data.Function          (on)
import           Data.Monad.State       ()
import           Data.Monad.Type
import           Data.Structure
import           Data.TyError
import           Data.Type
import           Infer.Levels
import           Infer.Monad
import           Infer.TypeMarshal

-- | Update the level of a type reference, or register the reference to have its
-- level updated eventually. Level changes are made in response to unification:
-- If a type is unified with a variable at a lower level (higher scope), then
-- its level must be duly updated to indicate that it is accessible from that
-- higher scope, and so should not be generalised at the lower scope.
updateLevel :: MonadInfer m => Level -> TyRef (World m) -> m ()
updateLevel lvl tr = do
  StratTy{ty, newLevel, oldLevel} <- readIRef tr
  case ty of
    VarTB (FreeV _)
      | Set lvl'@(Lvl _) <- newLevel ->
        when (lvl < lvl') $ setLevel tr $ Set lvl

    _ | Set lvl'@(Lvl _) <- newLevel
      , length ty == 0 ->
        when (lvl < lvl') $ setLevel tr $ Set lvl

    _ | Marked _         <- newLevel -> throwError . OccursE =<< resolveTyRef tr
    _ | Set lvl'@(Lvl _) <- newLevel -> do
        when (lvl < lvl') $ do
          when (lvl' == oldLevel) $ do
            delayLevelUpdate tr
          setLevel tr $ Set lvl
        return ()

    _ -> error "updateLevel: cannot update level"

-- | Massage two type references until they are both the 'same' (not counting
-- forwarding pointers).
unify :: MonadInfer m
      => TyRef (World m)
      -- ^ The expected type
      -> TyRef (World m)
      -- ^ The actual type
      -> m ()

unify _tr _ur = do
  [_tr, _ur] <- mapM repr [_tr, _ur]
  if _tr == _ur
  then return ()
  else do
    StratTy{ty = tyT, newLevel = lvlT} <- readIRef _tr
    StratTy{ty = tyU, newLevel = lvlU} <- readIRef _ur
    case (lvlT, lvlU) of
      (Set lt, Set lu) ->
        case (tyT, tyU) of
          (VarTB _, VarTB _)
            | lt < lu           -> link _tr _ur
            | otherwise         -> link _ur _tr
          (VarTB _, _)          -> updateLevel lt _ur >> link _ur _tr
          (_, VarTB _)          -> updateLevel lu _tr >> link _tr _ur
          _ | tyT `shapeEq` tyU -> do
            let minLvl = lt `min` lu
            mapM_ (markTy 0) [_tr, _ur]
            unifySub minLvl tyT tyU
            mapM_ (flip setLevel $ Set minLvl) [_tr, _ur]
          _ -> do
            [te, ta] <- mapM resolveTyRef [_tr, _ur]
            throwError $ UnificationE te ta Nothing
      (Marked _, _) -> throwError . OccursE =<< resolveTyRef _tr
      (_, Marked _) -> throwError . OccursE =<< resolveTyRef _ur
  where
    link vr wr = do
      modifyIRef wr $ \st -> st{ty = VarTB (FwdV vr)}

    unifySub l u v =
      let recur = sequence_ $ (zipWith (unifyLev l) `on` toList) u v
      in  catchError recur addUnifyCtx

    unifyLev l vr wr = do
      vp <- repr vr
      updateLevel l vp
      unify vp wr

    addUnifyCtx (UnificationE te ta _) = do
      [pte, pta] <- mapM resolveTyRef [_tr, _ur]
      throwError $ UnificationE te ta (Just (pte, pta))
    addUnifyCtx e = throwError e
