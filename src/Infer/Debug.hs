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
import           Infer.Monad

-- | Debug method to print the structure of a type as represented by type
-- references.
showTyRef :: MonadInfer m => TyRef (World m) -> m String
showTyRef _tr = evalStateT ((start++) <$> p [_tr]) S.empty
  where
    start = replicate 10 '=' ++ "\n"
    end   = replicate 10 '-' ++ "\n"

    p [] = return end
    p (_ur:rs) = do
     Ty {uid, subs} <- readIRef =<< repr _ur
     visited <- get
     if S.member uid visited then p rs
     else do
       modify (S.insert uid)
       subInfos <- mapM getSub $ maybe [] H.toList subs
       let line = show uid ++ ": " ++ intercalate " " subInfos ++ "\n"
       (line++) <$> p (allChildren subs ++ rs)

    getSub (ctr, (Sub _ cs)) = do
      uids <- mapM getUID cs
      return $ show ctr ++ "(" ++ intercalate ", " uids ++ ")"

    getUID = repr >=> readIRef >=> pure . show . uid
