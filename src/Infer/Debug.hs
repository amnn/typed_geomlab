{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Utility functions for debugging the type checker.

|-}
module Infer.Debug where

import           Control.Monad.ST.Class
import           Data.Foldable          (forM_)
import           Data.Monad.Type
import           Data.Type
import           Debug.Trace            (traceM)
import           Infer.Monad

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
        BoolTB   -> prn "bool"
        NumTB    -> prn "num"
        StrTB    -> prn "str"
        AtomTB   -> prn "atom"

        VarTB (FreeV n) -> prn $ "var: " ++ n
        VarTB (FwdV f)  -> prn "var: ~~>" >> p (off + 2) f

        ListTB t   -> prn "list: " >> indent t
        ArrTB as r -> do
          prn "fn: "
          forM_ as $ \a -> indent a >> prn "---"
          prn "-->"
          indent r
          prn "***"
