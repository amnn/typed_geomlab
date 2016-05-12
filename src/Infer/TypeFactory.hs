{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Ways to build types as represented internally by the type checker.

|-}
module Infer.TypeFactory where

import           Control.Monad.Reader
import           Control.Monad.ST.Class
import           Data.Flag
import qualified Data.HashMap.Strict    as H
import           Data.Monad.DynArray
import           Data.Monad.State
import           Data.Monad.Type
import           Infer.Monad

-- | Introduce a new local variable to the stack, and return a reference to it.
pushLocal :: MonadInferTop m => m (TyRef (World m))
pushLocal = do
  tyCtx <- getTyCtx
  tr    <- newVar
  liftST (push tyCtx tr)
  return tr

-- | Remove the top-most local variable from the stack.
popLocal :: MonadInferTop m => m ()
popLocal = getTyCtx >>= liftST . pop

-- | Produce a unique type id
fresh :: MonadInferTop m => m Int
fresh = do
  SS {gsRef}     <- ask
  GS {nextTyVar} <- readIRef gsRef
  modifyIRef gsRef bumpVar
  return nextTyVar
  where
    bumpVar is@GS {nextTyVar = n} = is {nextTyVar = n + 1}

-- | Create the subtype for the given constructor of a Remy encoding with fresh
-- variables and flag parameters.
freshSub :: MonadInferTop m => Flag -> Ctr -> m (Sub (World m))
freshSub flg ctr = Sub flg <$> replicateM (arity ctr) newVar

-- | Create a new type at the current level, from the structure provided.
newTy :: MonadInferTop m
      => H.HashMap Ctr (Sub (World m))
      -- ^ The structure of the type
      -> m (TyRef (World m))
      -- ^ A reference to a type with the given structure, created at the
      -- current level.

newTy subs = do
  lvl <- getCurrLvl
  uid <- fresh
  newIRef $ Ty { uid      = uid
               , subs     = subs
               , newLevel = Set (Lvl lvl)
               , oldLevel = Lvl lvl
               }

-- | Create a new type at the generic level, from the structure provided.
genTy :: MonadInferTop m
      => H.HashMap Ctr (Sub (World m))
      -> m (TyRef (World m))
genTy subs = do
  uid <- fresh
  newIRef $ Ty { uid      = uid
               , subs     = subs
               , newLevel = Set Gen
               , oldLevel = Gen
               }

-- | Create a new variable at the current level.
newVar :: MonadInferTop m
       => m (TyRef (World m))
newVar = freshSub dontCare Any >>= newTy . H.singleton Any

-- | Superset encoding for a single constructor.
sup :: MonadInferTop m => Ctr -> m (H.HashMap Ctr (Sub (World m)))
sup ctr = do
  anyS <- freshSub dontCare Any
  ctrS <- freshSub must ctr
  return $ H.fromList [(Any, anyS), (ctr, ctrS)]

-- | Subset encoding for a single constructor.
sub :: MonadInferTop m => Ctr -> m (H.HashMap Ctr (Sub (World m)))
sub ctr = do
  anyS <- freshSub mustNot Any
  ctrS <- freshSub dontCare ctr
  return $ H.fromList [(Any, anyS), (ctr, ctrS)]

setSub :: MonadInferTop m => TyRef (World m) -> Ctr -> Sub (World m) -> m ()
setSub _tr ctr s = do
  _tr <- repr _tr
  t@Ty {subs} <- readIRef _tr
  writeIRef _tr t { subs = H.insert ctr s subs}

getSub :: MonadInferTop m => TyRef (World m) -> Ctr -> m (Sub (World m))
getSub tr ctr = do
  Ty {subs} <- readIRef =<< repr tr
  case H.lookup ctr subs of
    Just s  -> return s
    Nothing -> do
      flg <- case wildcard ctr of
               Nothing   -> return dontCare
               Just wCtr -> flag <$> getSub tr wCtr
      s   <- freshSub flg ctr
      setSub tr ctr s
      return s
