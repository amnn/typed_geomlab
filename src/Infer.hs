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
import           Data.Constructor
import           Data.Expr
import           Data.Flag
import qualified Data.HashMap.Strict    as H
import           Data.Literal
import           Data.Location
import           Data.Monad.DynArray
import           Data.Monad.State
import           Data.Monad.Type
import           Data.Patt
import qualified Data.Set               as S
import           Data.Sugar
import           Data.TyError
import           Infer.Context
import           Infer.Debug            (showTyRef)
import           Infer.FlagTree
import           Infer.Generalise
import           Infer.Monad
import           Infer.TypeFactory
import           Infer.TypeMarshal
import           Infer.Unify

-- | Calculate the type of a given expression, given a map from free variables
-- to type references. Alternatively, an error may be thrown.
typeOf :: MonadInfer m => GloDef (World m) -> Expr -> m (TyRef (World m))
typeOf gloDefs = check
  where
    check (LitE (NumB _))    = sup  Num    >>= newTy
    check (LitE (StrB _))    = sup  Str    >>= newTy
    check (LitE (BoolB _))   = sup  Bool   >>= newTy
    check (LitE (AtomB a))   = sup (Tag a) >>= newTy
    check (LitE  NilB)       = sup  Nil    >>= newTy
    check (LitE (ConsB h t)) = do
      cr  <- newTy =<< sup Cons
      ers <- mapM check [h, t]
      Sub _ crs <- getSub cr Cons
      sequence_ (zipWith unify crs ers)
      return cr

    check (VarE ix) = getLocalTy ix >>= instantiate

    check (FreeE v)
      | Just trm <- H.lookup v gloDefs =
        case trm of
          Nothing -> throwError (DeferE v)
          Just tr -> instantiate tr
      | otherwise = throwError $ UnboundVarE v

    check (CaseE e as) = do
      -- Check case argument
      etr <- check e

      -- Unify types of arms, gathering constraints for case argument
      atr  <- newVar
      pats <- mapM (checkArm etr atr) as

      -- Build constraints
      mnf    <- contextualise mustNot
      notAny <- freshSub mnf Any
      let constraints = Just . H.fromList $ (Any, notAny):pats

      -- Constrain case argument
      ctr <- newTy constraints
      unify etr ctr

      return atr

    check (FnE a e) = do
      let ctr = Fn a
      ptrs <- replicateM a pushLocal
      etr  <- check e
      fr   <- newTy =<< sup ctr
      s    <- getSub fr ctr
      replicateM_ a popLocal

      -- decorrelations between parameters
      rtrs  <- mapM reachable ptrs
      let rUids = S.unions (snd <$> rtrs)
      forM_ rtrs $ \(trs, uids) -> do
        let rUids' = rUids `S.difference` uids
        forM_ trs $ \tr -> decorrelateTy tr rUids'

      setSub fr ctr s { children = etr:ptrs }
      return fr

    check (AppE f as) = do
      let ctr = Fn (length as)
      ftr             <- check f
      atrs            <- mapM check as
      arr             <- sub ctr >>= newTy
      Sub flg (rtr:_) <- getSub arr ctr
      setSub arr ctr (Sub flg (rtr:atrs))
      unify ftr arr
      return rtr

    check (LetE a b) = do
      atr <- newScope $ do
        ltr <- pushLocal
        unify ltr =<< check a
        return ltr
      (_, uids) <- reachable atr
      decorrelateTy atr uids
      generalise atr
      btr <- check b
      popLocal
      return btr

    check (LocE lbl le) =
      catchError (check (dislocate le)) $ \e -> do
        throwError $ CtxE lbl (le *> pure e)

    checkArm etr btr (p, a) = do
      let ctr = patCtr p
      ptrs <- replicateM (holes p) pushLocal

      atr  <- case ctr of
                Any -> check a
                _   -> inContext etr ctr (check a)

      unify btr atr
      replicateM_ (holes p) popLocal
      dcf <- contextualise dontCare
      return (ctr, Sub dcf ptrs)

    patCtr (ValPB (NumB _))    =  Num
    patCtr (ValPB (StrB _))    =  Str
    patCtr (ValPB (BoolB _))   =  Bool
    patCtr (ValPB (AtomB a))   = (Tag a)
    patCtr (ValPB (ConsB _ _)) =  Cons
    patCtr (ValPB  NilB)       =  Nil
    patCtr (VarPB  _)          =  Any

-- | Given a list of top-level statements, type check each one individually and
-- give the type or error for each top-level operation.
typeCheck :: [Para Expr] -> [Para (Either TyError String)]
typeCheck ps = runST $ flip runReaderT undefined $ do
  tyCtx <- liftST  $ newArray_ 4
  gsRef <- newIRef $ GS { tyCtx
                        , waitingToAdjust = []
                        , suspectTys      = []
                        , nextTyVar       = 0
                        }
  local (const $ SS {gsRef, lvl = 0, context = []}) $
    evalStateT (mapM tcPara ps) =<< initialDefs
  where

    topCtx (LocE lbl le) act =
      catchError act $ \e ->
        throwError (CtxE lbl (le *> pure e))
    topCtx _ _ =
      error "topCtx: No location at top level."

    topScope im = runExceptT im

    guardTy x im = catchError im $ \e -> do
      modify (H.insert x Nothing)
      throwError e

    tcPara (Eval e)  = fmap Eval . topScope $ get
                                          >>= flip typeOf e
                                          >>= showTyRef

    tcPara (Def x e) = fmap (Def x)
                     . topScope
                     . guardTy x $ do
      dtr <- newScope $ do
        evr <- newVar
        modify (H.insert x (Just evr))
        etr <- flip typeOf e =<< get
        topCtx e $ unify evr etr
        return evr
      (_, uids) <- reachable dtr
      decorrelateTy dtr uids
      generalise dtr
      showTyRef dtr
