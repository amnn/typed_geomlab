{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, PatternGuards, RankNTypes #-}
module Infer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.ST.Class (liftST)
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.HashMap as H
import Data.STRef
import Debug.Trace (traceM)
import DynArray
import Expr
import Literal
import Patt
import Structure (shapeEq)
import Sugar
import Token (Id)
import Type

type GSRef s = STRef s (GlobalState s)
type TyRef s = STRef s (StratTy s)

data GlobalState s = GS { tyCtx           :: DynArray s (TyRef s)
                        , waitingToAdjust :: [TyRef s]
                        , nextTyVar       :: !Int
                        }

data ScopedState s = SS { gsRef :: GSRef s
                        , lvl   :: !Int
                        }

data Level = Lvl Int | Gen deriving (Eq, Show, Ord)

data StratV  s = FreeV Id | FwdV (TyRef s) deriving Eq
data StratTy s = StratTy { ty       :: TyB (StratV s) (TyRef s)
                         , newLevel :: Maybe Level
                         , oldLevel :: !Level
                         } deriving Eq

type TyError  = String
type InferM s = ReaderT (ScopedState s) (ExceptT TyError (ST s))

newIRef :: a -> InferM s (STRef s a)
newIRef = liftST . newSTRef

readIRef :: STRef s a -> InferM s a
readIRef = liftST . readSTRef

writeIRef :: STRef s a -> a -> InferM s ()
writeIRef sr x = liftST (writeSTRef sr x)

modifyIRef :: STRef s a -> (a -> a) -> InferM s ()
modifyIRef sr f = liftST (modifySTRef sr f)

getCurrLvl :: InferM s Int
getCurrLvl = asks lvl

getTyCtx :: InferM s (DynArray s (TyRef s))
getTyCtx = tyCtx <$> (asks gsRef >>= readIRef)

bumpVar :: GlobalState s -> GlobalState s
bumpVar is@GS {nextTyVar = n} = is {nextTyVar = n + 1}

newScope :: ScopedState s -> ScopedState s
newScope st@SS{lvl = l} = st{lvl = l + 1}

delayTy :: TyRef s -> GlobalState s -> GlobalState s
delayTy tr is@GS {waitingToAdjust = wta} = is {waitingToAdjust = tr : wta}

getLocalTy :: Int -> InferM s (TyRef s)
getLocalTy ix = do
  GS {tyCtx} <- readIRef =<< asks gsRef
  liftST $ peek ix tyCtx

pushLocal :: InferM s (TyRef s)
pushLocal = do
  tyCtx <- getTyCtx
  tr    <- newVar
  liftST $ push tyCtx tr
  return tr

popLocal :: InferM s ()
popLocal = getTyCtx >>= liftST . pop

fresh :: InferM s Id
fresh = do
  SS {gsRef}     <- ask
  GS {nextTyVar} <- readIRef gsRef
  modifyIRef gsRef bumpVar
  return (toId nextTyVar)
  where
    a = ord 'a'
    toId x
      | x < 26    = [chr (a + x)]
      | otherwise = 't' : show (x - 26)

newTy :: TyB (StratV s) (TyRef s) -> InferM s (TyRef s)
newTy t = do
  lvl <- getCurrLvl
  newIRef $ StratTy { ty = t, newLevel = Just (Lvl lvl), oldLevel = Lvl lvl }

genTy :: TyB (StratV s) (TyRef s) -> InferM s (TyRef s)
genTy t = newIRef $ StratTy { ty = t, newLevel = Just Gen, oldLevel = Gen }

newVar :: InferM s (TyRef s)
newVar = fresh >>= newTy . VarTB . FreeV

delayLevelUpdate :: TyRef s -> InferM s ()
delayLevelUpdate tr = do
  SS {gsRef} <- ask
  modifyIRef gsRef (delayTy tr)

printTyRef :: TyRef s -> InferM s ()
printTyRef = p 0
  where
    p off tr = do
      let spcs = replicate off ' '
      let prn  = traceM . (spcs ++)
      StratTy{ty, newLevel, oldLevel} <- readIRef tr
      prn $ concat ["{", show newLevel, ", ", show oldLevel, "}"]
      case ty of
        BoolTB -> prn "bool"
        NumTB  -> prn "num"
        StrTB  -> prn "str"
        AtomTB -> prn "atom"

        VarTB (FreeV n) -> prn $ "var: " ++ n
        VarTB (FwdV f)  -> prn "var: ~~>" >> p (off + 2) f

        ListTB t   -> prn "list: " >> p (off + 2) t
        ArrTB as r -> do
          prn "fn: "
          forM_ as $ \a -> p (off + 2) a >> prn "---"
          prn "-->"
          p (off + 2) r
          prn "***"

-- | Resolves a type to its concrete representation. If the type is a variable
-- and that variable is a forwarding pointer to some other type, follow the
-- pointers until a concrete type is reached, and compress the path followed.
repr :: TyRef s -> InferM s (TyRef s)
repr tr = do
  sty@StratTy{ty} <- readIRef tr
  case ty of
    VarTB (FwdV _tr) -> do
      _tr <- repr _tr
      writeIRef tr $ sty {ty = (VarTB (FwdV _tr))}
      return _tr
    _ -> return tr

markTy :: TyRef s -> InferM s Level
markTy tr = do
  sty@StratTy{newLevel = Just lvl} <- readIRef tr
  writeIRef tr sty {newLevel = Nothing}
  return lvl

setLevel :: TyRef s -> Level -> InferM s ()
setLevel tr lvl = do
  modifyIRef tr $ \st -> st{newLevel = Just lvl}

getLevel :: TyRef s -> InferM s Level
getLevel tr = do
  StratTy{newLevel = Just lvl} <- readIRef tr
  return lvl

unifyLevels :: TyRef s -> Level -> InferM s ()
unifyLevels tr lvl =
  modifyIRef tr $ \st -> st{newLevel = Just lvl, oldLevel = lvl}

cycleFree :: TyRef s -> InferM s ()
cycleFree tr = do
  StratTy{ty, newLevel} <- readIRef tr
  case ty of
    VarTB (FwdV t) -> cycleFree t
    _ | Nothing <- newLevel -> error "cycle: occurs check"
    _ -> do lvl <- markTy tr
            mapM_ cycleFree ty
            setLevel tr lvl

updateLevel :: Level -> TyRef s -> InferM s ()
updateLevel lvl tr = do
  StratTy{ty, newLevel, oldLevel} <- readIRef tr
  case ty of
    VarTB (FreeV _)
      | Just lvl'@(Lvl _) <- newLevel ->
        when (lvl < lvl') $ setLevel tr lvl

    _ | Just lvl'@(Lvl _) <- newLevel
      , length ty == 0 ->
        when (lvl < lvl') $ setLevel tr lvl

    _ | Nothing           <- newLevel -> error "cycle: occurs check"
    _ | Just lvl'@(Lvl _) <- newLevel -> do
        when (lvl < lvl') $ do
          when (lvl' == oldLevel) $ do
            delayLevelUpdate tr
          setLevel tr lvl
        return ()

    _ -> error "cannot update level"

unify :: TyRef s -> TyRef s -> InferM s ()
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
          _ -> error "unification error"
      _ -> error "cycle: occurs check"
  where
    link vr wr = do
      modifyIRef wr $ \st -> st{ty = VarTB (FwdV vr)}

    unifySub l u v = sequence_ $ (zipWith (unifyLev l) `on` toList) u v

    unifyLev l vr wr = do
      vp <- repr vr
      updateLevel l vp
      unify vp wr

forceDelayedAdjustments :: InferM s ()
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
        Nothing    -> error "cycle: occurs check"
        Just nLvl' -> do
          when (nLvl' > nLvl) (setLevel _tr nLvl)
          adjustTop ts _tr

generalise :: TyRef s -> InferM s ()
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

instantiate :: TyRef s -> InferM s (TyRef s)
instantiate tRef = do
  substR <- newIRef H.empty
  inst substR tRef
  where
    inst substR tr = do
      StratTy{ty, newLevel = Just lvl} <- readIRef tr
      case ty of
        VarTB (FreeV n) | Gen <- lvl -> do
          subst <- readIRef substR
          case H.lookup n subst of
            Just ty' -> return ty'
            Nothing  -> do
              nr <- newVar
              modifyIRef substR (H.insert n nr)
              return nr
        VarTB (FwdV link) -> inst substR link
        _ | Gen <- lvl    -> mapM (inst substR) ty >>= newTy
        _                 -> return tr

typeOf :: H.Map Id (TyRef s) -> Expr -> InferM s (TyRef s)
typeOf gloDefs = check
  where
    check (LitE l)  = checkLit check l
    check (VarE ix) = getLocalTy ix >>= instantiate

    check (FreeE v)
      | Just tr <- H.lookup v gloDefs = instantiate tr
      | otherwise                     = error ("unbound free variable: " ++ v)

    check (IfE c t e) = do
      [ctr, ttr, tte] <- mapM check [c, t, e]
      btr <- newTy BoolTB
      unify ctr btr
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
      atr <- local newScope $ do
        ltr <- pushLocal
        check a >>= unify ltr
        return ltr
      generalise atr
      btr <- check b
      popLocal
      return btr

    check (SeqE a b) = check a >> check b

    check _ = newVar

    checkArm etr (p, a) = do
      patTy p >>= unify etr
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

resolveTyRef :: TyRef s -> InferM s (Ty Id)
resolveTyRef tr = do
  StratTy{ty} <- readIRef =<< repr tr
  case ty of
    VarTB (FreeV n) -> return $ VarT n
    VarTB (FwdV  _) -> error "resolveTyRef: forward pointer!"

    BoolTB          -> return $ BoolT
    NumTB           -> return $ NumT
    StrTB           -> return $ StrT
    AtomTB          -> return $ AtomT
    ListTB t        -> ListT <$> resolveTyRef t
    ArrTB as b      -> ArrT <$> mapM resolveTyRef as <*> resolveTyRef b

abstractTy :: (Ty Id) -> InferM s (TyRef s)
abstractTy ty = do
  sr <- newIRef H.empty
  absT sr ty
  where
    absT sr (VarT n) = do
      subst <- readIRef sr
      case H.lookup n subst of
        Just vr -> return vr
        Nothing -> do
          vr <- genTy $ (VarTB (FreeV n))
          modifyIRef sr (H.insert n vr)
          return vr

    absT _  BoolT       = genTy $ BoolTB
    absT _  NumT        = genTy $ NumTB
    absT _  StrT        = genTy $ StrTB
    absT _  AtomT       = genTy $ AtomTB
    absT sr (ListT t)   = absT sr t >>= genTy . ListTB
    absT sr (ArrT as b) = do
      atrs <- mapM (absT sr) as
      bs   <- absT sr b
      genTy (ArrTB atrs bs)

initialDefs :: InferM s (H.Map Id (TyRef s))
initialDefs = H.fromList <$> mapM absDef ts
  where
    absDef (n, ty) = do { tr <- abstractTy ty; return (n, tr) }
    numBOp i = (i, ArrT [NumT, NumT] NumT)
    numMOp i = (i, ArrT [NumT] NumT)
    relBOp i = (i, ArrT [VarT "a", VarT "a"] BoolT)
    ts = (numBOp <$> ["+", "-", "*", "/"])
      ++ (numMOp <$> ["~", "int"])
      ++ (relBOp <$> ["<", "<=", "<>", "=", ">=", ">"])
      ++ [ ("numeric", ArrT [NumT] BoolT)
         , (":",       ArrT [VarT "a", ListT (VarT "a")] (ListT (VarT "a")))
         , ("true",    BoolT)
         , ("false",   BoolT)
         ]

typeCheck :: [Para Expr] -> Either TyError [Ty Id]
typeCheck ps = runST (runExceptT (runReaderT topLevel undefined))
  where
    topLevel = do
      gloDefs <- initialDefs
      reverse . snd <$> foldM tcPara (gloDefs, []) ps

    topScope im = do
      tyCtx <- liftST $ newArray_ 4
      gsRef <- newIRef $ GS {tyCtx, waitingToAdjust = [], nextTyVar = 0}
      local (const $ SS {gsRef, lvl = 0}) im

    tcPara (gloDefs, ts) (Eval e)  = topScope $ do
      etr <- typeOf gloDefs e
      cycleFree etr
      eft   <- resolveTyRef etr
      return (gloDefs, eft:ts)

    tcPara (gloDefs, ts) (Def x e) = topScope $ do
      (gloDefs', dtr) <- local newScope $ do
        evr <- newVar
        let gloDefs' = H.insert x evr gloDefs
        etr <- typeOf gloDefs' e
        unify evr etr
        cycleFree evr
        return (gloDefs', evr)
      generalise dtr
      eft <- resolveTyRef dtr
      return (gloDefs', eft:ts)
