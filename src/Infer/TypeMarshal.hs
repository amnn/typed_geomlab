{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Bringing types in and out of the type checker's internal representation.

|-}
module Infer.TypeMarshal where

import           Control.Monad.ST.Class
import           Control.Monad.State
import qualified Data.HashMap           as H
import           Data.Monad.State
import           Data.Monad.Type
import           Data.Token             (Id)
import           Data.Type
import           Infer.Levels
import           Infer.Monad
import           Infer.TypeFactory

-- | Convert a type reference into a type tree by chasing forwarding pointers, and pruning
-- cycles we may encounter to prevent infinite-depth trees.
resolveTyRef :: MonadInfer m => TyRef (World m) -> m (Ty Id)
resolveTyRef _tr = do
  StratTy{newLevel} <- readIRef =<< repr _tr
  case newLevel of
    Marked m -> resolve (m + 1) _tr
    Set _    -> resolve 0       _tr
  where
    resolve m _tr = do
      _tr <- repr _tr
      StratTy{ty, newLevel} <- readIRef _tr
      afterMarking m _tr $
        case newLevel of
          Marked n | n >= m -> return (VarT "*")
          _                 -> recur m ty

    recur _ (VarTB (FreeV n)) = return $ VarT n
    recur _ (VarTB (FwdV  _)) = error "resolveTyRef: forward pointer!"

    recur _ BoolTB   = return $ BoolT
    recur _ NumTB    = return $ NumT
    recur _ StrTB    = return $ StrT
    recur _ AtomTB   = return $ AtomT

    recur m (ListTB t)   = ListT <$> resolve m t
    recur m (ArrTB as b) = ArrT  <$> mapM (resolve m) as <*> resolve m b

-- | Create a type reference at the "general" level, from a type tree.
abstractTy :: MonadST m => Ty Id -> m (TyRef (World m))
abstractTy ty = evalStateT (absT ty) H.empty
  where
    absT (VarT n) = do
      subst <- get
      case H.lookup n subst of
        Just vr -> return vr
        Nothing -> do
          vr <- genTy . VarTB . FreeV $ n
          put (H.insert n vr subst)
          return vr

    absT BoolT       = genTy $ BoolTB
    absT NumT        = genTy $ NumTB
    absT StrT        = genTy $ StrTB
    absT AtomT       = genTy $ AtomTB
    absT (ListT t)   = absT t >>= genTy . ListTB

    absT (ArrT as b) = do
      atrs <- mapM absT as
      btr  <- absT b
      genTy (ArrTB atrs btr)

-- | Types of operations and values that are already provided by the language.
initialDefs :: MonadST m => m (GloDef (World m))
initialDefs = H.fromList <$> mapM absDef ts
  where
    absDef (n, ty) = do { tr <- abstractTy ty; return (n, Just tr) }
    numBOp i = (i, ArrT [NumT, NumT] NumT)
    numMOp i = (i, ArrT [NumT] NumT)
    relBOp i = (i, ArrT [VarT "a", VarT "a"] BoolT)
    ts = (numBOp <$> ["+", "-", "*", "/"])
      ++ (numMOp <$> ["~", "int"])
      ++ (relBOp <$> ["<", "<=", "<>", "=", ">=", ">", "and", "or"])
      ++ [ ("numeric", ArrT [VarT "a"] BoolT)
         , (":",       ArrT [VarT "a", ListT (VarT "a")] (ListT (VarT "a")))
         , ("true",    BoolT)
         , ("false",   BoolT)
         , ("_debug",  ArrT [] BoolT)
         , ("_print",  ArrT [StrT] (ListT (VarT "a")))
         ]
