{-|

State managed by the type checker during the course of type inference.

|-}
module Data.Monad.State where

import           Data.Constructor
import qualified Data.HashMap.Strict as H
import           Data.Monad.DynArray
import           Data.Monad.Type
import           Data.STRef
import           Data.Token          (Id)

type GSRef  s = STRef s (GlobalState s)
type GloDef s = H.HashMap Id (Maybe (TyRef s))

-- | This state is held in a reference that is available from anywhere in the
-- type checker. It holds the list of currently bound variables, a list of type
-- references whose levels need to be fully adjusted, and a counter used to
-- spawn new type variables.
data GlobalState s = GS { tyCtx           :: DynArray s (TyRef s)
                        , waitingToAdjust :: [TyRef s]
                        , nextTyVar       :: !Int
                        }

-- | Read-only environment state. Holds the reference to the global state (which
-- never changes), the current level, which increases as we move through more
-- let expressions, and the case context, which is scoped by case expressions.
data ScopedState s = SS { gsRef   :: GSRef s
                        , lvl     :: !Int
                        , context :: [(TyRef s, Ctr)]
                        }
