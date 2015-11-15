{-# LANGUAGE NamedFieldPuns, PatternGuards #-}
module Infer where

import Control.Monad (foldM, foldM_, replicateM, replicateM_, when)
import Control.Monad.ST
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.HashMap as H
import Data.STRef
import DynArray
import Expr
import Literal
import Patt
import Structure (shapeEq)
import Sugar
import Token (Id)
import Type

type ISRef s = STRef s (InferState s)
type TyRef s = STRef s (StratTy s)
data InferState s = IS { tyCtx           :: DynArray s (TyRef s)
                       , waitingToAdjust :: [TyRef s]
                       , nextTyVar       :: !Int
                       }

data Level = Lvl Int | Gen deriving (Eq, Show, Ord)

data StratV  s = FreeV Id | FwdV (TyRef s) deriving Eq
data StratTy s = StratTy { ty       :: TyB (StratV s) (TyRef s)
                         , newLevel :: Maybe Level
                         , oldLevel :: !Level
                         } deriving Eq

bumpVar :: InferState s -> InferState s
bumpVar is@IS {nextTyVar = n} = is {nextTyVar = n + 1}

delayTy :: TyRef s -> InferState s -> InferState s
delayTy tr is@IS {waitingToAdjust = wta} = is {waitingToAdjust = tr : wta}

getLocalTy :: ISRef s -> Int -> ST s (TyRef s)
getLocalTy isRef ix = do
  IS {tyCtx} <- readSTRef isRef
  peek ix tyCtx

pushLocal :: ISRef s -> Int -> ST s (TyRef s)
pushLocal isRef lvl = do
  IS {tyCtx} <- readSTRef isRef
  tr <- newVar isRef lvl
  push tyCtx tr
  return tr

popLocal :: ISRef s -> ST s ()
popLocal isRef = do
  IS {tyCtx} <- readSTRef isRef
  pop tyCtx

fresh :: ISRef s -> ST s Id
fresh isRef = do
  IS {nextTyVar} <- readSTRef isRef
  modifySTRef isRef bumpVar
  return (toId nextTyVar)
  where
    a = ord 'a'
    toId x
      | x < 26    = [chr (a + x)]
      | otherwise = 't' : show (x - 26)

newTy :: Int -> TyB (StratV s) (TyRef s) -> ST s (TyRef s)
newTy lvl t = newSTRef $ StratTy { ty = t, newLevel = Just (Lvl lvl), oldLevel = Lvl lvl }

genTy :: TyB (StratV s) (TyRef s) -> ST s (TyRef s)
genTy t = newSTRef $ StratTy { ty = t, newLevel = Just Gen, oldLevel = Gen }

newVar :: ISRef s -> Int -> ST s (TyRef s)
newVar isRef lvl = do
  v <- fresh isRef
  newTy lvl (VarTB (FreeV v))

delayLevelUpdate :: ISRef s -> TyRef s -> ST s ()
delayLevelUpdate isRef tr = modifySTRef isRef (delayTy tr)

-- | Resolves a type to its concrete representation. If the type is a variable
-- and that variable is a forwarding pointer to some other type, follow the
-- pointers until a concrete type is reached, and compress the path followed.
repr :: TyRef s -> ST s (TyRef s)
repr tr = do
  sty@StratTy{ty} <- readSTRef tr
  case ty of
    VarTB (FwdV _tr) -> do
      _tr <- repr _tr
      writeSTRef tr $ sty {ty = (VarTB (FwdV _tr))}
      return _tr
    _ -> return tr

markTy :: TyRef s -> ST s Level
markTy tr = do
  sty@StratTy{newLevel = Just lvl} <- readSTRef tr
  writeSTRef tr sty {newLevel = Nothing}
  return lvl

setLevel :: TyRef s -> Level -> ST s ()
setLevel tr lvl = do
  modifySTRef tr $ \st -> st{newLevel = Just lvl}

getLevel :: TyRef s -> ST s Level
getLevel tr = do
  StratTy{newLevel = Just lvl} <- readSTRef tr
  return lvl

unifyLevels :: TyRef s -> Level -> ST s ()
unifyLevels tr lvl =
  modifySTRef tr $ \st -> st{newLevel = Just lvl, oldLevel = lvl}

cycleFree :: TyRef s -> ST s ()
cycleFree tr = do
  StratTy{ty, newLevel} <- readSTRef tr
  case ty of
    VarTB (FwdV t) -> cycleFree t
    _ | Nothing <- newLevel -> error "cycle: occurs check"
    _ -> do lvl <- markTy tr
            mapM_ cycleFree ty
            setLevel tr lvl

updateLevel :: ISRef s -> Level -> TyRef s -> ST s ()
updateLevel isRef lvl tr = do
  StratTy{ty, newLevel, oldLevel} <- readSTRef tr
  case ty of
    VarTB (FreeV _)
      | Just lvl'@(Lvl _) <- newLevel -> do
        when (lvl < lvl') $ setLevel tr lvl
        return ()

    _ | Nothing           <- newLevel -> error "cycle: occurs check"
    _ | Just lvl'@(Lvl _) <- newLevel -> do
        when (lvl < lvl') $ do
          when (lvl' == oldLevel) $ do
            delayLevelUpdate isRef tr
          setLevel tr lvl
        return ()

    _ -> error "cannot update level"

unify :: ISRef s -> TyRef s -> TyRef s -> ST s ()
unify isRef _tr _ur = do
  [_tr, _ur] <- mapM repr [_tr, _ur]
  if _tr == _ur
  then return ()
  else do
    StratTy{ty = tyT, newLevel = lvlT} <- readSTRef _tr
    StratTy{ty = tyU, newLevel = lvlU} <- readSTRef _ur
    case (lvlT, lvlU) of
      (Just lt, Just lu) ->
        case (tyT, tyU) of
          (VarTB _, VarTB _)
            | lt > lu           -> link _tr _ur
            | otherwise         -> link _ur _tr
          (VarTB _, _)          -> updateLevel isRef lt _ur >> link _ur _tr
          (_, VarTB _)          -> updateLevel isRef lu _tr >> link _tr _ur
          _ | tyT `shapeEq` tyU -> do
            let minLvl = lt `min` lu
            mapM_ markTy [_tr, _ur]
            unifySub minLvl tyT tyU
            mapM_ (flip setLevel minLvl) [_tr, _ur]
          _ -> error "unification error"
      _ -> error "cycle: occurs check"
  where
    link vr wr = do
      modifySTRef wr $ \st -> st{ty = VarTB (FwdV vr)}

    unifySub l u v = sequence_ $ (zipWith (unifyLev l) `on` toList) u v

    unifyLev l vr wr = do
      vp <- repr vr
      updateLevel isRef l vp
      unify isRef vp wr

forceDelayedAdjustments :: ISRef s -> Level -> ST s ()
forceDelayedAdjustments isRef lvl = do
  is@IS{waitingToAdjust} <- readSTRef isRef
  delayed <- foldM adjustTop [] waitingToAdjust
  writeSTRef isRef is{waitingToAdjust = delayed}
  where
    adjustTop ts tr = do
      StratTy{ty, newLevel = Just nLvl, oldLevel = oLvl} <- readSTRef tr
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
      StratTy{newLevel = mNLvl'} <- readSTRef _tr
      case mNLvl' of
        Nothing    -> error "cycle: occurs check"
        Just nLvl' -> do
          when (nLvl' > nLvl) (setLevel _tr nLvl)
          adjustTop ts _tr

generalise :: ISRef s -> Int -> TyRef s -> ST s ()
generalise isRef lvl tr = do
  forceDelayedAdjustments isRef currLvl
  gen tr
  where
    currLvl = Lvl lvl
    gen _ur = do
      _ur <- repr _ur
      StratTy{ty, newLevel = Just l} <- readSTRef _ur
      when (l > currLvl) $
        case ty of
          VarTB (FreeV _)    -> setLevel _ur Gen
          _ | length ty == 0 -> return ()
            | otherwise -> do
            reps <- mapM repr ty
            mapM_ gen reps
            lvls <- mapM getLevel ty
            let maxLvl = maximum lvls
            unifyLevels _ur maxLvl

instantiate :: ISRef s -> Int -> TyRef s -> ST s (TyRef s)
instantiate isRef currLvl tRef = do
  substR <- newSTRef H.empty
  inst substR tRef
  where
    inst substR tr = do
      StratTy{ty, newLevel = Just lvl} <- readSTRef tr
      case ty of
        VarTB (FreeV n) | Gen <- lvl -> do
          subst <- readSTRef substR
          case H.lookup n subst of
            Just ty' -> return ty'
            Nothing  -> do
              nr <- newVar isRef currLvl
              modifySTRef substR (H.insert n nr)
              return nr
        VarTB (FwdV link) -> inst substR link
        _ | Gen <- lvl -> do
          ty' <- mapM (inst substR) ty
          newTy currLvl ty'
        _ -> return tr


typeOf :: H.Map Id (TyRef s) -> ISRef s -> Int -> Expr -> ST s (TyRef s)
typeOf gloDefs isRef = check
  where
    check lvl (LitE l)  = checkLit (check lvl) lvl l
    check lvl (VarE ix) = getLocalTy isRef ix >>= instantiate isRef lvl

    check lvl (FreeE v)
      | Just tr <- H.lookup v gloDefs = instantiate isRef lvl tr
      | otherwise                     = error ("unbound free variable: " ++ v)

    check lvl (IfE c t e) = do
      [ctr, ttr, tte] <- mapM (check lvl) [c, t, e]
      btr <- newTy lvl BoolTB
      unify isRef ctr btr
      unify isRef ttr tte
      return ttr

    check lvl (CaseE e as) = do
      etr        <- check lvl e
      (atr:atrs) <- mapM (checkArm lvl etr) as
      foldM_ (const $ unify isRef atr) () atrs
      return atr

    check lvl (FnE a e) = do
      ptrs <- replicateM a (pushLocal isRef lvl)
      etr  <- check lvl e
      replicateM_ a (popLocal isRef)
      newTy lvl (ArrTB ptrs etr)

    check lvl (AppE f as) = do
      ftr  <- check lvl f
      atrs <- mapM (check lvl) as
      rtr  <- newVar isRef lvl
      ftr' <- newTy lvl (ArrTB atrs rtr)
      unify isRef ftr ftr'
      return rtr

    check lvl (LetE a b) = do
      ltr <- pushLocal isRef lvl
      check (lvl+1) a >>= unify isRef ltr
      generalise isRef lvl ltr
      btr <- check lvl b
      popLocal isRef
      return btr

    check lvl (SeqE a b) = check lvl a >> check lvl b

    check lvl _ = newVar isRef lvl

    checkArm lvl etr (p, a) = do
      patTy lvl p >>= unify isRef etr
      atr <- check lvl a
      replicateM_ (holes p) (popLocal isRef)
      return atr

    patTy lvl (ValPB l) = checkLit (const $ pushLocal isRef lvl) lvl l
    patTy lvl (VarPB _) = pushLocal isRef lvl

    checkLit _ lvl (NumB _)    = newTy lvl NumTB
    checkLit _ lvl (StrB _)    = newTy lvl StrTB
    checkLit _ lvl (AtomB _)   = newTy lvl AtomTB
    checkLit _ lvl NilB        = newVar isRef lvl >>= newTy lvl . ListTB
    checkLit f lvl (ConsB h t) = do
      [htr, ttr] <- mapM f [h, t]
      ltr <- newTy lvl (ListTB htr)
      unify isRef ltr ttr
      return ltr

resolveTyRef :: TyRef s -> ST s FixTy
resolveTyRef tr = do
  StratTy{ty} <- readSTRef =<< repr tr
  case ty of
    VarTB (FreeV n) -> return $ FixTy (VarTB n)
    VarTB (FwdV  _) -> error "resolveTyRef: forward pointer!"

    BoolTB          -> return $ FixTy BoolTB
    NumTB           -> return $ FixTy NumTB
    StrTB           -> return $ FixTy StrTB
    AtomTB          -> return $ FixTy AtomTB
    ListTB t        -> FixTy . ListTB <$> resolveTyRef t
    ArrTB as b      -> FixTy <$> (ArrTB <$> mapM resolveTyRef as <*> resolveTyRef b)

abstractTy :: FixTy -> ST s (TyRef s)
abstractTy ft = do
  sr <- newSTRef H.empty
  absT sr ft
  where
    absT sr (FixTy (VarTB n)) = do
      subst <- readSTRef sr
      case H.lookup n subst of
        Just vr -> return vr
        Nothing -> do
          vr <- genTy $ (VarTB (FreeV n))
          modifySTRef sr (H.insert n vr)
          return vr

    absT _  (FixTy BoolTB)       = genTy $ BoolTB
    absT _  (FixTy NumTB)        = genTy $ NumTB
    absT _  (FixTy StrTB)        = genTy $ StrTB
    absT _  (FixTy AtomTB)       = genTy $ AtomTB
    absT sr (FixTy (ListTB t))   = absT sr t >>= genTy . ListTB
    absT sr (FixTy (ArrTB as b)) = do
      atrs <- mapM (absT sr) as
      bs   <- absT sr b
      genTy (ArrTB atrs bs)

initialDefs :: ST s (H.Map Id (TyRef s))
initialDefs = H.fromList <$> mapM absDef ts
  where
    absDef (n, ty) = do { tr <- abstractTy ty; return (n, tr) }
    ts = [ ("+", FixTy (ArrTB [FixTy NumTB, FixTy NumTB] (FixTy NumTB)))
         , ("-", FixTy (ArrTB [FixTy NumTB, FixTy NumTB] (FixTy NumTB)))
         , ("~", FixTy (ArrTB [FixTy NumTB] (FixTy NumTB)))
         , (":", FixTy (ArrTB [FixTy (VarTB "a"), FixTy (ListTB (FixTy (VarTB "a")))] (FixTy (ListTB (FixTy (VarTB "a"))))))
         , ("true",  FixTy BoolTB)
         , ("false", FixTy BoolTB)
         ]

typeCheck :: [Para Expr] -> [FixTy]
typeCheck ps = runST $ do
  tyCtx   <- newArray_ 4
  isRef   <- newSTRef $ IS {tyCtx, waitingToAdjust = [], nextTyVar = 0}
  gloDefs <- initialDefs
  reverse . snd <$> foldM (tcPara isRef) (gloDefs, []) ps
  where
    tcPara isRef (gloDefs, ts) (Eval e)  = do
      etr <- typeOf gloDefs isRef 0 e
      cycleFree etr
      eft <- resolveTyRef etr
      return (gloDefs, eft:ts)

    tcPara isRef (gloDefs, ts) (Def x e) = do
      evr <- newVar isRef 1
      let gloDefs' = H.insert x evr gloDefs
      etr <- typeOf gloDefs' isRef 1 e
      unify isRef evr etr
      cycleFree evr
      generalise isRef 0 evr
      eft <- resolveTyRef evr
      return (gloDefs', eft:ts)
