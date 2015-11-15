module InferSpec where

import SpecHelper
import Type

spec :: Spec
spec = do
  typeCheckFile "test/compose.geom" $
    [ (FixTy (ArrTB [ FixTy (ArrTB [ FixTy (VarTB "b")]
                                   ( FixTy (VarTB "c")))
                    , FixTy (ArrTB [ FixTy (VarTB "a")]
                                   ( FixTy (VarTB "b")))
                    ]
                    (  FixTy (ArrTB [ FixTy (VarTB "a")]
                                    ( FixTy (VarTB "c"))))))
    , FixTy (ArrTB [ FixTy NumTB]
                   ( FixTy NumTB))

    , FixTy (ArrTB [ FixTy NumTB]
                   ( FixTy NumTB))
    ]
