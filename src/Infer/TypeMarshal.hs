{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Bringing types in and out of the type checker's internal representation.

|-}
module Infer.TypeMarshal where

import           Control.Monad.ST.Class
import           Control.Monad.State
import qualified Data.HashMap.Strict    as H
import           Data.Monad.State
import           Data.Monad.Type        (Sub(..))
import qualified Data.Monad.Type        as MT
import qualified Data.Type              as T
import           Infer.Monad
import           Infer.TypeFactory

-- | From a type tree, create a Remy type at the "general" level, for the subset
-- of the given type.
loadTy :: MonadInferTop m => T.Ty -> m (MT.TyRef (World m))
loadTy ty = evalStateT (ldSup ty) H.empty
  where
    lookupVar v = do
      subst <- get
      case H.lookup v subst of
        Just vr -> return vr
        Nothing -> do
          vr <- genTy Nothing
          put (H.insert v vr subst)
          return vr

    ldSup (T.VarT v)    = lookupVar v
    ldSup  T.BoolT      = sup  MT.Bool   >>= genTy
    ldSup  T.NumT       = sup  MT.Num    >>= genTy
    ldSup  T.StrT       = sup  MT.Str    >>= genTy
    ldSup  T.AtomT      = sup  MT.Atom   >>= genTy
    ldSup  T.NilT       = sup  MT.Nil    >>= genTy
    ldSup (T.TagT t)    = sup (MT.Tag t) >>= genTy

    ldSup (T.ConsT a b) = do
      tr  <- genTy =<< sup MT.Cons
      crs <- mapM ldSup [a, b]
      s   <- getSub tr MT.Cons
      setSub tr MT.Cons s { children = crs }
      return tr

    ldSup (T.ArrT as b) = do
      let ctr = MT.Fn (length as)
      tr   <- genTy =<< sub ctr
      atrs <- mapM ldSub as
      btr  <- ldSup b
      s    <- getSub tr ctr
      setSub tr ctr s { children = btr:atrs}
      return tr

    ldSub (T.VarT v)    = lookupVar v
    ldSub  T.BoolT      = sub  MT.Bool   >>= genTy
    ldSub  T.NumT       = sub  MT.Num    >>= genTy
    ldSub  T.StrT       = sub  MT.Str    >>= genTy
    ldSub  T.AtomT      = sub  MT.Atom   >>= genTy
    ldSub  T.NilT       = sub  MT.Nil    >>= genTy
    ldSub (T.TagT t)    = sub (MT.Tag t) >>= genTy

    ldSub (T.ConsT a b) = do
      tr  <- genTy =<< sub MT.Cons
      crs <- mapM ldSub [a, b]
      s   <- getSub tr MT.Cons
      setSub tr MT.Cons s { children = crs }
      return tr

    ldSub (T.ArrT as b) = do
      let ctr = MT.Fn (length as)
      tr   <- genTy =<< sub ctr
      atrs <- mapM ldSup as
      btr  <- ldSub b
      s    <- getSub tr ctr
      setSub tr ctr s { children = btr:atrs}
      return tr

-- | Types of operations and values that are already provided by the language.
initialDefs :: MonadInferTop m => m (GloDef (World m))
initialDefs = H.fromList <$> mapM absDef ts
  where
    absDef (n, ty) = do { tr <- loadTy ty; return (n, Just tr) }
    numBOp i = (i, T.ArrT [T.NumT, T.NumT] T.NumT)
    numMOp i = (i, T.ArrT [T.NumT] T.NumT)
    relBOp i = (i, T.ArrT [T.VarT "a", T.VarT "a"] T.BoolT)
    ts = (numBOp <$> ["+", "-", "*", "/"])
      ++ (numMOp <$> ["~", "int"])
      ++ (relBOp <$> ["<", "<=", "<>", "=", ">=", ">", "and", "or"])
      ++ [ ("numeric", T.ArrT [T.VarT "a"] T.BoolT)
         , (":",       T.ArrT [T.VarT "a", T.VarT "b"] (T.ConsT (T.VarT "a") (T.VarT "b")))
         , ("true",    T.BoolT)
         , ("false",   T.BoolT)
         ]
