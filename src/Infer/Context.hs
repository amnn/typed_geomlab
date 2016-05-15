{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

{-|

Operations to manipulate the current context and create flag parameters with
respect to it.

|-}
module Infer.Context where

import           Control.Monad.Reader
import           Control.Monad.ST.Class
import           Data.Constructor
import           Data.Flag
import qualified Data.HashMap.Strict    as H
import           Data.Monad.State
import           Data.Monad.Type
import           Infer.Monad

-- | Introduce a new piece of information into the case context.
inContext :: MonadInferTop m => TyRef (World m) -> Ctr -> m a -> m a
inContext tr ctr act = local addToCtx act
  where
    addToCtx ss@SS {context} =
      ss { context = (tr, ctr):context }

-- | Create a flag tree that is interpreted as the given flag in the current
-- context, as a "don't care" value in other contexts.
contextualise :: MonadInferTop m => Flag -> m (FlagTree (World m))
contextualise f = foldl buildTree (FL f) <$> asks context
  where
    buildTree child (tr, ctr) =
      FT { caseArg = tr
         , interp  = dontCare
         , arms    = H.singleton ctr child
         }
