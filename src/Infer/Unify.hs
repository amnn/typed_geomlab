{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

{-|

Type Unification

|-}
module Infer.Unify where

import           Control.Monad.Except
import           Control.Monad.ST.Class
import           Data.Constructor
import           Data.Function          (on)
import qualified Data.HashMap.Strict    as H
import           Data.Monad.State       ()
import           Data.Monad.Type
import qualified Data.Set               as S
import           Infer.FlagTree
import           Infer.Levels
import           Infer.Monad
import           Infer.TypeFactory

frontier :: H.HashMap Ctr a -> H.HashMap Ctr a -> S.Set Ctr
frontier = S.union `on` (S.fromList . H.keys)

-- | Update the level of a type reference, or register the reference to have its
-- level updated eventually. Level changes are made in response to unification:
-- If a type is unified with a variable at a lower level (higher scope), then
-- its level must be duly updated to indicate that it is accessible from that
-- higher scope, and so should not be generalised at the lower scope.
updateLevel :: MonadInfer m => Level -> TyRef (World m) -> m ()
updateLevel lvl tr = do
  Ty {subs, newLevel, oldLevel} <- readIRef tr
  case newLevel of
    Marked _ -> return ()

    Set lvl'@(Lvl _)
      | null (allChildren subs) ->
          when (lvl < lvl') $ setLevel tr $ Set lvl

      | otherwise -> do
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
  when (_tr /= _ur) $ do
    Ty {subs = subT, newLevel = Set lt} <- readIRef _tr
    Ty {subs = subU, newLevel = Set lu} <- readIRef _ur
    case (subT, subU) of
      (Nothing, _) -> _tr ~> _ur
      (_, Nothing) -> _ur ~> _tr
      (Just st, Just su) -> do
        let ctrs = S.toList (frontier st su)
        subPairs <- mapM (subPair _tr _ur) ctrs
        _ur ~> _tr
        updateLevel (lt `min` lu) _tr
        mapM_ (unifySub _tr) subPairs
  where
    _ur ~> _tr = writeIRef _ur (Fwd _tr)

    subPair _tr _ur ctr = (ctr,,) <$> getSub _tr ctr <*> getSub _ur ctr

    unifyChildren = zipWith unify `on` children

    unifySub _tr (ctr, tSub, uSub) = do
      flag' <- merge _tr (flag tSub) (flag uSub)
      setSub _tr ctr (tSub { flag = flag' })
      sequence_ (unifyChildren tSub uSub)
