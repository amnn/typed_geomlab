{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Shape of the Monad that type inference runs in, and a suite of basic operations
it provides.

|-}
module Infer.Monad where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Control.Monad.State
import           Data.Monad.DynArray
import           Data.Monad.State
import           Data.Monad.Type
import qualified Data.Set               as S
import           Data.STRef
import           Data.TyError

-- | Constraints of monads used to setup the type checker.
type MonadInferTop m = ( MonadST m
                       , MonadReader (ScopedState (World m)) m
                       )

-- | Constraint of all the Monads used to check individual expressions.
type MonadInfer m = ( MonadInferTop m
                    , MonadError TyError m
                    )

-- | Concrete Monad Transformer Stack satisfying the @ MonadInfer @
-- constraint. @ StateT (GloDef s) @ is introduced so as to keep track of the
-- global definition map without storing a reference to it.
type InferM s = ReaderT (ScopedState s)
              ( ExceptT TyError
              ( StateT (GloDef s)
              ( ST s)))

-- The usual STRef operations, lifted to any monad that supports ST operations.
newIRef :: MonadST m => a -> m (STRef (World m) a)
newIRef = liftST . newSTRef

readIRef :: MonadST m => STRef (World m) a -> m a
readIRef = liftST . readSTRef

writeIRef :: MonadST m => STRef (World m) a -> a -> m ()
writeIRef sr x = liftST (writeSTRef sr x)

modifyIRef :: MonadST m => STRef (World m) a -> (a -> a) -> m ()
modifyIRef sr f = liftST (modifySTRef sr f)

-- | Query the environment for the current level.
getCurrLvl :: MonadInferTop m => m Int
getCurrLvl = asks lvl

-- | Query the global state for the list of local variables.
getTyCtx :: MonadInferTop m => m (DynArray (World m) (TyRef (World m)))
getTyCtx = tyCtx <$> (asks gsRef >>= readIRef)

-- | Query the environment for the type of a particular local variable,
-- identified by its deBruijn index.
getLocalTy :: MonadInferTop m => Int -> m (TyRef (World m))
getLocalTy ix = getTyCtx >>= liftST . peek ix

-- | Add a type reference to the list of types suspected of causing errors.
addSuspect :: MonadInferTop m => TyRef (World m) -> m ()
addSuspect tr = do { gs<- asks gsRef; modifyIRef gs doAdd }
  where
    doAdd gs@GS {suspectTys} =
      gs { suspectTys = tr : suspectTys }

-- | Run the supplied monad in a scope nested one level below the current one.
newScope :: MonadInferTop m => m a -> m a
newScope = local bumpScope
  where bumpScope st@SS {lvl = l} = st{lvl = l + 1}

-- | Resolves a type to its concrete representation. If the type is a variable
-- and that variable is a forwarding pointer to some other type, follow the
-- pointers until a concrete type is reached, and compress the path followed.
repr :: MonadInferTop m => TyRef (World m) -> m (TyRef (World m))
repr tr = do
  ty <- readIRef tr
  case ty of
    Fwd _pr -> do
      _pr <- repr _pr
      writeIRef tr $ Fwd _pr
      return _pr
    _ -> return tr

-- | Return a list of all the type uids that are reachable from the given
-- type reference.
reachable :: MonadInferTop m => TyRef (World m) -> m (S.Set Int)
reachable tr = execStateT (go tr) S.empty
  where
    go _tr = do
      _tr            <- repr _tr
      reached        <- get
      Ty {uid, subs} <- readIRef _tr
      when (S.notMember uid reached) $ do
        modify (S.insert uid)
        mapM_ go (allChildren subs)
