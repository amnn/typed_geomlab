{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

{-|

Utility functions for debugging the type checker.

|-}
module Infer.Debug where

import           Control.Monad.ST.Class
import           Control.Monad.State
import qualified Data.HashMap.Strict    as H
import           Data.List              (intercalate)
import           Data.Monad.Type
import qualified Data.Set               as S
import           Debug.Trace            (traceM)
import           Infer.Monad

-- | Debug method to print the structure of a type as represented by type
-- references.
printTyRef :: MonadInfer m => TyRef (World m) -> m ()
printTyRef _tr = evalStateT (p [_tr]) S.empty
  where
    p [] = return ()
    p (_ur:rs) = do
     Ty {uid, subs} <- readIRef =<< repr _ur
     visited <- get
     when (S.notMember uid visited) $ do
       modify (S.insert uid)
       subInfos <- mapM getSub $ maybe [] H.toList subs
       traceM $ show uid ++ ": " ++ intercalate " " subInfos
       p $ allChildren subs ++ rs

    getSub (ctr, (Sub flg cs)) = do
      uids <- mapM getUID cs
      return $ show ctr ++ show flg ++ "(" ++ intercalate ", " uids ++ ")"

    getUID = repr >=> readIRef >=> pure . show . uid
