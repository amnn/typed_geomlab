module TyError where

import Location
import Token

-- | Possible Type Errors that the type checker may emit
data TyError = UnboundVarE Id
            -- ^ A free variable was found for which no type exists
            -- (i.e. undefined variable).
             | UnificationE
            -- ^ Could not unify two types.
             | OccursE
            -- ^ When unifying a variable with a type, we found the variable
            -- within the type (A cycle was detected in the occurs check).
             | CtxE String (Located TyError)
            -- ^ Adds context to an error in the form of a label and a location.
               deriving (Eq, Show)
