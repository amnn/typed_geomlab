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
import           Data.Foldable          (toList)
import           Data.Function          (on)
import qualified Data.HashMap           as H
import           Data.Literal
import           Data.Location
import           Data.Monad.DynArray
import           Data.Monad.State
import           Data.Monad.Type
import           Data.Patt
import           Data.Structure         (shapeEq)
import           Data.Sugar
import           Data.Token             (Id)
import           Data.TyError
import           Data.Type
import           Infer.Levels
import           Infer.Monad
import           Infer.TypeFactory
import           Infer.TypeMarshal

-- | Check whether a type is free of all cycles. This function is only used at
-- the top-level.
cycleFree :: MonadInfer m => TyRef (World m) -> m ()
cycleFree tr = do
  StratTy{ty, newLevel} <- readIRef tr
  case ty of
    VarTB (FwdV t) -> cycleFree t
    _ | Marked _ <- newLevel -> throwError . OccursE =<< resolveTyRef tr
    _ -> do afterMarking 0 tr $ mapM_ cycleFree ty

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
      StratTy{ty, newLevel = Set nLvl, oldLevel = oLvl} <- readIRef tr
      case ty of
        _ | oLvl <= lvl  -> return (tr:ts)
        _ | oLvl == nLvl -> return ts
        _ -> do
          Set mLvl <- markTy 0 tr
          ts'  <- foldM (adjustRec nLvl) ts ty
          unifyLevels tr mLvl
          return ts'

    adjustRec nLvl ts _tr = do
      _tr <- repr _tr
      StratTy{newLevel = mNLvl'} <- readIRef _tr
      case mNLvl' of
        Marked _  -> throwError . OccursE =<< resolveTyRef _tr
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
      StratTy{ty, newLevel = Set l} <- readIRef _ur
      when (l > lvl) $
        case ty of
          VarTB (FreeV _)    -> setLevel _ur $ Set Gen
          _ | length ty == 0 -> return ()
            | otherwise -> do
            reps <- mapM repr ty
            mapM_ gen reps
            lvls <- mapM getLevel reps
            let maxLvl = maximum lvls
            unifyLevels _ur maxLvl

-- | Replace universally quantified variables in a (possibly) general type with
-- fresh type variables at the current level: Every instance of a generalised
-- type is new, and unification with one instance should not affect another.
instantiate :: MonadInfer m => TyRef (World m) -> m (TyRef (World m))
instantiate tRef = evalStateT (inst tRef) H.empty
  where
    inst tr = do
      StratTy{ty, newLevel = Set lvl} <- readIRef tr
      case ty of
        VarTB (FreeV n) | Gen <- lvl -> do
          subst <- get
          case H.lookup n subst of
            Just ty' -> return ty'
            Nothing  -> do
              nr <- newVar
              put (H.insert n nr subst)
              return nr
        VarTB (FwdV link) -> inst link
        _ | Gen <- lvl    -> mapM inst ty >>= newTy
        _                 -> return tr

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
