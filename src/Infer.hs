{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , NamedFieldPuns
           , PatternGuards
           , RankNTypes #-}

module Infer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.State
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.HashMap as H
import Data.STRef
import Debug.Trace (traceM)
import DynArray
import Expr
import Literal
import Location
import Patt
import Structure (shapeEq)
import Sugar
import Token (Id)
import TyError
import Type

type GSRef  s = STRef s (GlobalState s)
type TyRef  s = STRef s (StratTy s)
type GloDef s = H.Map Id (TyRef s)

-- | This state is held in a reference that is available from anywhere in the
-- type checker. It holds the list of currently bound variables, a list of type
-- references whose levels need to be fully adjusted, and a counter used to
-- spawn new type variables.
data GlobalState s = GS { tyCtx           :: DynArray s (TyRef s)
                        , waitingToAdjust :: [TyRef s]
                        , nextTyVar       :: !Int
                        }

-- | Read-only environment state. Holds the reference to the global state (which
-- never changes) and the current level, which increases as we move through more
-- @ let @ expressions.
data ScopedState s = SS { gsRef :: GSRef s
                        , lvl   :: !Int
                        }

-- | A representation of the level of a type, with a notion of ordering. @ Gen @
-- represents the "generalised" level, which is considered higher than all other
-- levels.
data Level = Lvl Int | Gen deriving (Eq, Show, Ord)

-- | Representation of variables. Each can either be a name, or a link to
-- another type reference.
data StratV  s = FreeV Id | FwdV (TyRef s) deriving Eq

-- | Representation of types, annotated by their level.
data StratTy s = StratTy { ty       :: TyB (StratV s) (TyRef s)
                         , newLevel :: Maybe Level
                         , oldLevel :: !Level
                         } deriving Eq

-- | Constraint of all the Monads used by the type checker.
type MonadInfer m = ( MonadST m
                    , MonadReader (ScopedState (World m)) m
                    , MonadError TyError m
                    )

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
getCurrLvl :: MonadInfer m => m Int
getCurrLvl = asks lvl

-- | Query the global state for the list of local variables.
getTyCtx :: MonadInfer m => m (DynArray (World m) (TyRef (World m)))
getTyCtx = tyCtx <$> (asks gsRef >>= readIRef)

-- | Query the environment for the type of a particular local variable,
-- identified by its deBruijn index.
getLocalTy :: MonadInfer m => Int -> m (TyRef (World m))
getLocalTy ix = getTyCtx >>= liftST . peek ix

-- | Introduce a new local variable to the stack, and return a reference to it.
pushLocal :: MonadInfer m => m (TyRef (World m))
pushLocal = do
  tyCtx <- getTyCtx
  tr    <- newVar
  liftST $ push tyCtx tr
  return tr

-- | Remove the top-most local variable from the stack.
popLocal :: MonadInfer m => m ()
popLocal = getTyCtx >>= liftST . pop

-- | Run the supplied monad in a scope nested one level below the current one.
newScope :: MonadInfer m => m a -> m a
newScope = local bumpScope
  where bumpScope st@SS {lvl = l} = st{lvl = l + 1}

-- | Produce a unique variable name.
fresh :: MonadInfer m => m Id
fresh = do
  SS {gsRef}     <- ask
  GS {nextTyVar} <- readIRef gsRef
  modifyIRef gsRef bumpVar
  return (toId nextTyVar)
  where
    bumpVar is@GS {nextTyVar = n} = is {nextTyVar = n + 1}

    a = ord 'a'
    toId x
      | x < 26    = [chr (a + x)]
      | otherwise = 't' : show (x - 26)

-- | Create a new type at the current level, from the structure provided.
newTy :: MonadInfer m
      => TyB (StratV (World m)) (TyRef (World m))
      -- ^ The structure of the type
      -> m (TyRef (World m))
      -- ^ A reference to a type with the given structure, created at the
      -- current level.

newTy t = do
  lvl <- getCurrLvl
  newIRef $ StratTy { ty = t, newLevel = Just (Lvl lvl), oldLevel = Lvl lvl }

-- | Create a new type at the generic level, from the structure provided.
genTy :: MonadST m => TyB (StratV (World m)) (TyRef (World m)) -> m (TyRef (World m))
genTy t = newIRef $ StratTy { ty = t, newLevel = Just Gen, oldLevel = Gen }

-- | Create a fresh type variable at the current level.
newVar :: MonadInfer m => m (TyRef (World m))
newVar = fresh >>= newTy . VarTB . FreeV

-- | Add a type reference to the global list of references waiting to have their
-- levels adjusted.
delayLevelUpdate :: MonadInfer m => TyRef (World m) -> m ()
delayLevelUpdate tr = do
  SS {gsRef} <- ask
  modifyIRef gsRef (delayTy tr)
  where
    delayTy t is@GS {waitingToAdjust = wta} = is {waitingToAdjust = t : wta}

-- | Debug method to print the structure of a type as represented by type
-- references. This function may diverge if given a cyclic type structure.
printTyRef :: MonadInfer m => TyRef (World m) -> m ()
printTyRef = p 0
  where
    p off tr = do
      let spcs = replicate off ' '
      let prn  = traceM . (spcs ++)
      let indent = p (off + 2)
      StratTy{ty, newLevel, oldLevel} <- readIRef tr
      prn $ concat ["{", show newLevel, ", ", show oldLevel, "}"]
      case ty of
        BoolTB -> prn "bool"
        NumTB  -> prn "num"
        StrTB  -> prn "str"
        AtomTB -> prn "atom"

        VarTB (FreeV n) -> prn $ "var: " ++ n
        VarTB (FwdV f)  -> prn "var: ~~>" >> p (off + 2) f

        RefTB  t   -> prn "ref: "  >> indent t
        ListTB t   -> prn "list: " >> indent t
        HashTB k v -> prn "hash: " >> indent k >> prn "==>" >> indent v
        ArrTB as r -> do
          prn "fn: "
          forM_ as $ \a -> indent a >> prn "---"
          prn "-->"
          indent r
          prn "***"

-- | Resolves a type to its concrete representation. If the type is a variable
-- and that variable is a forwarding pointer to some other type, follow the
-- pointers until a concrete type is reached, and compress the path followed.
repr :: MonadInfer m => TyRef (World m) -> m (TyRef (World m))
repr tr = do
  sty@StratTy{ty} <- readIRef tr
  case ty of
    VarTB (FwdV _tr) -> do
      _tr <- repr _tr
      writeIRef tr $ sty {ty = (VarTB (FwdV _tr))}
      return _tr
    _ -> return tr

-- | Mark a reference as having been visited. Doing so overwrites its level
-- value, so that is returned so that it may be restored later. If a marked
-- reference is visited more than once, this indicates a cycle has been
-- detected.
markTy :: MonadInfer m => TyRef (World m) -> m Level
markTy tr = do
  sty@StratTy{newLevel = Just lvl} <- readIRef tr
  writeIRef tr sty {newLevel = Nothing}
  return lvl

-- | Set the level of a type reference.
setLevel :: MonadInfer m => TyRef (World m) -> Level -> m ()
setLevel tr lvl = do
  modifyIRef tr $ \st -> st{newLevel = Just lvl}

-- | Get the level of a type reference.
getLevel :: MonadInfer m => TyRef (World m) -> m Level
getLevel tr = do
  StratTy{newLevel = Just lvl} <- readIRef tr
  return lvl

-- | Set all level values of a type as the same. When this is done to non-leaf
-- types, it indicates that all child levels have been properly adjusted and
-- accounted for.
unifyLevels :: MonadInfer m => TyRef (World m) -> Level -> m ()
unifyLevels tr lvl =
  modifyIRef tr $ \st -> st{newLevel = Just lvl, oldLevel = lvl}

-- | Check whether a type is free of all cycles. This function is only used at
-- the top-level.
cycleFree :: MonadInfer m => TyRef (World m) -> m ()
cycleFree tr = do
  StratTy{ty, newLevel} <- readIRef tr
  case ty of
    VarTB (FwdV t) -> cycleFree t
    _ | Nothing <- newLevel -> throwError OccursE
    _ -> do lvl <- markTy tr
            mapM_ cycleFree ty
            setLevel tr lvl

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
      | Just lvl'@(Lvl _) <- newLevel ->
        when (lvl < lvl') $ setLevel tr lvl

    _ | Just lvl'@(Lvl _) <- newLevel
      , length ty == 0 ->
        when (lvl < lvl') $ setLevel tr lvl

    _ | Nothing           <- newLevel -> throwError OccursE
    _ | Just lvl'@(Lvl _) <- newLevel -> do
        when (lvl < lvl') $ do
          when (lvl' == oldLevel) $ do
            delayLevelUpdate tr
          setLevel tr lvl
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
      (Just lt, Just lu) ->
        case (tyT, tyU) of
          (VarTB _, VarTB _)
            | lt > lu           -> link _tr _ur
            | otherwise         -> link _ur _tr
          (VarTB _, _)          -> updateLevel lt _ur >> link _ur _tr
          (_, VarTB _)          -> updateLevel lu _tr >> link _tr _ur
          _ | tyT `shapeEq` tyU -> do
            let minLvl = lt `min` lu
            mapM_ markTy [_tr, _ur]
            unifySub minLvl tyT tyU
            mapM_ (flip setLevel minLvl) [_tr, _ur]
          _ -> do
            [te, ta] <- mapM resolveTyRef [_tr, _ur]
            throwError $ UnificationE te ta Nothing
      _ -> throwError OccursE
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
      StratTy{ty, newLevel = Just nLvl, oldLevel = oLvl} <- readIRef tr
      case ty of
        _ | oLvl <= lvl  -> return (tr:ts)
        _ | oLvl == nLvl -> return ts
        _ -> do
          mLvl <- markTy tr
          ts'  <- foldM (adjustRec nLvl) ts ty
          unifyLevels tr mLvl
          return ts'

    adjustRec nLvl ts _tr = do
      _tr <- repr _tr
      StratTy{newLevel = mNLvl'} <- readIRef _tr
      case mNLvl' of
        Nothing    -> throwError OccursE
        Just nLvl' -> do
          when (nLvl' > nLvl) (setLevel _tr nLvl)
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
      StratTy{ty, newLevel = Just l} <- readIRef _ur
      when (l > lvl) $
        case ty of
          VarTB (FreeV _)    -> setLevel _ur Gen
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
      StratTy{ty, newLevel = Just lvl} <- readIRef tr
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
      | Just tr <- H.lookup v gloDefs = instantiate tr
      | otherwise                     = throwError $ UnboundVarE v

    check (IfE c t e) = do
      [ctr, ttr, tte] <- mapM check [c, t, e]
      btr <- newTy BoolTB
      unify btr ctr
      unify ttr tte
      return ttr

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

    check (SeqE a b) = check a >> check b

    check (LocE lbl le) =
      catchError (check (dislocate le)) $ \e -> do
        throwError $ CtxE lbl (le *> pure e)

    check _ = newVar

    checkArm etr (p, a) = do
      unify etr =<< patTy p
      atr <- check a
      replicateM_ (holes p) popLocal
      return atr

    patTy (ValPB l) = checkLit (const pushLocal) l
    patTy (VarPB _) = pushLocal

    checkLit _ (NumB _)    = newTy NumTB
    checkLit _ (StrB _)    = newTy StrTB
    checkLit _ (AtomB _)   = newTy AtomTB
    checkLit _ NilB        = newVar >>= newTy . ListTB
    checkLit f (ConsB h t) = do
      [htr, ttr] <- mapM f [h, t]
      ltr <- newTy (ListTB htr)
      unify ltr ttr
      return ltr

-- | Convert a type reference into a type tree by chasing forwarding pointers.
resolveTyRef :: MonadInfer m => TyRef (World m) -> m (Ty Id)
resolveTyRef tr = do
  StratTy{ty} <- readIRef =<< repr tr
  case ty of
    VarTB (FreeV n) -> return $ VarT n
    VarTB (FwdV  _) -> error "resolveTyRef: forward pointer!"

    BoolTB          -> return $ BoolT
    NumTB           -> return $ NumT
    StrTB           -> return $ StrT
    AtomTB          -> return $ AtomT

    RefTB  t        -> RefT  <$> resolveTyRef t
    ListTB t        -> ListT <$> resolveTyRef t
    HashTB k v      -> HashT <$> resolveTyRef k <*> resolveTyRef v
    ArrTB as b      -> ArrT  <$> mapM resolveTyRef as <*> resolveTyRef b

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
    absT (RefT  t)   = absT t >>= genTy . RefTB
    absT (ListT t)   = absT t >>= genTy . ListTB

    absT (HashT k v) = do
      ktr <- absT k
      vtr <- absT v
      genTy (HashTB ktr vtr)

    absT (ArrT as b) = do
      atrs <- mapM absT as
      btr  <- absT b
      genTy (ArrTB atrs btr)

-- | Types of operations and values that are already provided by the language.
initialDefs :: MonadST m => m (GloDef (World m))
initialDefs = H.fromList <$> mapM absDef ts
  where
    absDef (n, ty) = do { tr <- abstractTy ty; return (n, tr) }
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
         , ("_new",    ArrT [VarT "a"] (RefT (VarT "a")))
         , ("_get",    ArrT [RefT (VarT "a")] (VarT "a"))
         , ("_set",    ArrT [RefT (VarT "a"), VarT "a"] (VarT "a"))
         , ("_hash",   ArrT [] (HashT (VarT "k") (VarT "v")))
         , ("_lookup", ArrT [HashT (VarT "k") (VarT "v"), VarT "k"] (VarT "v"))
         , ("_update", ArrT [HashT (VarT "k") (VarT "v"), VarT "k", VarT "v"] (VarT "v"))
         , ("_syntax", HashT AtomT (VarT "v"))
         , ("_debug",  ArrT [] BoolT)
         , ("_print",  ArrT [StrT] (ListT (VarT "a")))
         ]

-- | Concrete Monad Transformer Stack satisfying the @ MonadInfer @
-- constraint. @ StateT (GloDef s) @ is introduced so as to keep track of the
-- global definition map without storing a reference to it.
type InferM s = ReaderT (ScopedState s)
              ( ExceptT TyError
              ( StateT (GloDef s)
              ( ST s)))

-- | Given a list of top-level statements, type check each one individually and
-- give the type or error for each top-level operation.
typeCheck :: [Para Expr] -> [Para (Either TyError (Ty Id))]
typeCheck ps = runST $ evalStateT (mapM tcPara ps) =<< initialDefs
  where
    topScope im = runExceptT . flip runReaderT undefined $ do
      tyCtx <- liftST $ newArray_ 4
      gsRef <- newIRef $ GS {tyCtx, waitingToAdjust = [], nextTyVar = 0}
      local (const $ SS {gsRef, lvl = 0}) im

    tcPara (Eval e) = fmap Eval . topScope $ do
      etr <- flip typeOf e =<< get
      cycleFree etr
      resolveTyRef etr

    tcPara (Def x e) = fmap (Def x) . topScope $ do
      dtr <- newScope $ do
        evr <- newVar
        modify (H.insert x evr)
        etr <- flip typeOf e =<< get
        unify evr etr
        cycleFree evr
        return evr
      generalise dtr
      resolveTyRef dtr
