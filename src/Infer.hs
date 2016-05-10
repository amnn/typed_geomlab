{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE RankNTypes       #-}

module Infer where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Control.Monad.State
import           Data.Expr
import qualified Data.HashMap           as H
import           Data.Literal
import           Data.Location
import           Data.Monad.DynArray
import           Data.Monad.State
import           Data.Monad.Type
import           Data.Patt
import           Data.Sugar
import           Data.Token             (Id)
import           Data.TyError
import           Data.Type
import           Infer.Generalise
import           Infer.Levels
import           Infer.Monad
import           Infer.TypeFactory
import           Infer.TypeMarshal
import           Infer.Unify

-- | Check whether a type is free of all cycles. This function is only used at
-- the top-level.
cycleFree :: MonadInfer m => TyRef (World m) -> m ()
cycleFree tr = do
  StratTy{ty, newLevel} <- readIRef tr
  case ty of
    VarTB (FwdV t) -> cycleFree t
    _ | Marked _ <- newLevel -> throwError . OccursE =<< resolveTyRef tr
    _ -> do afterMarking 0 tr $ mapM_ cycleFree ty

-- | Calculate the type of a given expression, given a map from free variables
-- to type references. Alternatively, an error may be thrown.
typeOf :: MonadInfer m => GloDef (World m) -> Expr -> m (TyRef (World m))
typeOf gloDefs = check
  where
    check (LitE l)  = checkLit check l
    check (VarE ix) = getLocalTy ix >>= instantiate

    check (FreeE v)
      | Just trm <- H.lookup v gloDefs =
        case trm of
          Nothing -> throwError (DeferE v)
          Just tr -> instantiate tr
      | otherwise = throwError $ UnboundVarE v

    check (CaseE e as) = do
      etr        <- check e
      (atr:atrs) <- mapM (checkArm etr) as
      foldM_ (const $ unify atr) () atrs
      return atr

    check (FnE a e) = do
      ptrs <- replicateM a pushLocal
      etr  <- check e
      replicateM_ a popLocal
      newTy (ArrTB ptrs etr)

    check (AppE f as) = do
      ftr  <- check f
      atrs <- mapM check as
      rtr  <- newVar
      unify ftr =<< newTy (ArrTB atrs rtr)
      return rtr

    check (LetE a b) = do
      atr <- newScope $ do
        ltr <- pushLocal
        unify ltr =<< check a
        cycleFree ltr
        return ltr
      generalise atr
      btr <- check b
      popLocal
      return btr

    check (LocE lbl le) =
      catchError (check (dislocate le)) $ \e -> do
        throwError $ CtxE lbl (le *> pure e)

    checkArm etr (p, a) = do
      unify etr =<< patTy p
      atr <- check a
      replicateM_ (holes p) popLocal
      return atr

    patTy (ValPB l) = checkLit (const pushLocal) l
    patTy (VarPB _) = pushLocal

    checkLit _ (NumB _)    = newTy NumTB
    checkLit _ (StrB _)    = newTy StrTB
    checkLit _ (BoolB _)   = newTy BoolTB
    checkLit _ (AtomB _)   = newTy AtomTB
    checkLit _ NilB        = newVar >>= newTy . ListTB
    checkLit f (ConsB h t) = do
      [htr, ttr] <- mapM f [h, t]
      ltr <- newTy (ListTB htr)
      unify ltr ttr
      return ltr

-- | Given a list of top-level statements, type check each one individually and
-- give the type or error for each top-level operation.
typeCheck :: [Para Expr] -> [Para (Either TyError (Ty Id))]
typeCheck ps = runST $ evalStateT (mapM tcPara ps) =<< initialDefs
  where
    topCtx (LocE lbl le) act =
      catchError act $ \e ->
        throwError (CtxE lbl (le *> pure e))
    topCtx _ _ =
      error "topCtx: No location at top level."

    topScope im = runExceptT . flip runReaderT undefined $ do
      tyCtx <- liftST $ newArray_ 4
      gsRef <- newIRef $ GS {tyCtx, waitingToAdjust = [], nextTyVar = 0}
      local (const $ SS {gsRef, lvl = 0}) im

    guardTy x im = catchError im $ \e -> do
      modify (H.insert x Nothing)
      throwError e

    tcPara (Eval e) = fmap Eval
                    . topScope $ do
      etr <- flip typeOf e =<< get
      topCtx e $ cycleFree etr
      resolveTyRef etr

    tcPara (Def x e) = fmap (Def x)
                     . topScope
                     . guardTy x $ do
      dtr <- newScope $ do
        evr <- newVar
        modify (H.insert x (Just evr))
        etr <- flip typeOf e =<< get
        topCtx e $ do
          unify evr etr
          cycleFree evr
        return evr
      generalise dtr
      resolveTyRef dtr
