{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

{-|

Utility functions for debugging the type checker.

|-}
module Infer.Debug where

import           Control.Monad.ST.Class
import           Control.Monad.State
import           Data.Constructor
import           Data.Function          (on)
import qualified Data.HashMap.Strict    as H
import           Data.List              (intercalate)
import           Data.List              (sortBy)
import           Data.Monad.Type
import qualified Data.Set               as S
import           Infer.Monad

-- | Debug method to print the structure of a type as represented by type
-- references.
showTyRef :: MonadInferTop m => TyRef (World m) -> m String
showTyRef _tr = evalStateT (unlines . (start:) <$> p [_tr]) S.empty
  where
    start = replicate 10 '='
    end   = replicate 10 '-'

    p [] = return [end]
    p (_ur:rs) = do
     Ty {uid, subs} <- readIRef =<< repr _ur
     visited <- get
     if S.member uid visited then p rs
     else do
       modify (S.insert uid)
       subInfos <- mapM getSub
                 . sortBy (compare `on` fst)
                 . maybe [] H.toList $ subs
       let fstLbl = show uid
       let indent = replicate (2 + length fstLbl) ' '
       let output = case subInfos of
                      []     -> [fstLbl]
                      (l:ls) -> (fstLbl ++ ": " ++ l) : map (indent++) ls
       (output++) <$> p (allChildren subs ++ rs)

    getSub (ctr, Sub f cs) = do
      uids    <- mapM getUID cs
      flagStr <- showFlagTree f
      return (flagStr ++ " ::: " ++ showCtr ctr uids)

    getUID = repr >=> readIRef >=> return . show . uid

    showCtr (Fn _) (r:ps) = "("++ intercalate ", " ps ++ ") -> " ++ r
    showCtr  Cons  [a, b] = a ++ ":" ++ b
    showCtr  ctr   _      = show ctr

showFlagTree :: MonadInferTop m => FlagTree (World m) -> m String
showFlagTree FL {interp} = return (show interp)

showFlagTree FT {caseArg, arms, interp} = do
  Ty {uid} <- readIRef =<< repr caseArg
  armStrs  <- fmtArms <$> traverse showFlagTree arms
  return $ show uid ++ show interp ++ "{" ++ armStrs ++ "}"
  where
    fmtArms              = intercalate ", "  . map fmtArm . H.toList
    fmtArm (ctr, armStr) = show ctr ++ " => " ++ armStr
