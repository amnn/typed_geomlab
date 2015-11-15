module InferSpec where

import SpecHelper
import Type

spec :: Spec
spec = do
  typeCheckFile "test/compose.geom" $
    [ ArrT [ ArrT [VarT "b"] (VarT "c")
           , ArrT [VarT "a"] (VarT "b")
           ]
           ( ArrT [VarT "a"] (VarT "c"))

    , ArrT [NumT] NumT
    , ArrT [NumT] NumT
    ]
