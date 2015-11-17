module InferSpec where

import SpecHelper
import Type

spec :: Spec
spec = do
  typeCheckFile "test/compose.geom" $
    Right
      [ ArrT [ ArrT [VarT "b"] (VarT "c")
             , ArrT [VarT "a"] (VarT "b")
             ]
             ( ArrT [VarT "a"] (VarT "c"))

      , ArrT [NumT] NumT
      , ArrT [NumT] NumT
      ]

  typeCheckFile "test/divmod.geom" $
    Right
      [ ArrT [NumT, NumT] NumT
      , ArrT [NumT, NumT] NumT

      , NumT
      , NumT

      , NumT
      , NumT
      ]

  typeCheckFile "test/empty.geom" $
    Right
      [ ArrT [] NumT
      ]

  typeCheckFile "test/folds.geom" $
    Right
      [ ArrT [ArrT [VarT "a", VarT "b"] (VarT "b"), VarT "b", ListT (VarT "a")] (VarT "b")
      , ArrT [ArrT [VarT "b", VarT "a"] (VarT "b"), VarT "b", ListT (VarT "a")] (VarT "b")
      , ArrT [ArrT [VarT "a"] (VarT "b"), ListT (VarT "a")] (ListT (VarT "b"))
      , ArrT [ArrT [VarT "a"] BoolT, ListT (VarT "a")] (ListT (VarT "a"))
      , ArrT [ListT (VarT "a")] NumT
      , ArrT [ListT (VarT "a")] (ListT (VarT "a"))
      ]

  typeCheckFile "test/let_gen.geom" $
    Right
      [ ArrT [VarT "a"] (VarT "a")
      , NumT
      , AtomT
      ]

  typeCheckFile "test/list_comp.geom" $
    Right
      [ ArrT [ArrT [VarT "a", VarT "b"] (VarT "b"), ListT (VarT "a"), VarT "b"] (VarT "b")
      , ArrT [NumT, NumT] (ListT NumT)

      , NumT
      , NumT

      , ListT (ListT StrT)
      , ListT (StrT)

      , BoolT

      , ListT NumT
      , ListT StrT
      , ListT (ListT StrT)
      ]

  typeCheckFile "test/monop_fn.geom" $
    Right [NumT, NumT, NumT]

  typeCheckFile "test/neg.geom" $
    Right [NumT, NumT, NumT, NumT]

  typeCheckFile "test/nest.geom" $
    Right [NumT]

  typeCheckFile "test/not.geom" $
    Right
      [ ArrT [BoolT] BoolT
      , BoolT
      , BoolT
      ]

  typeCheckFile "test/section.geom" $
    Right
      [ ArrT [ArrT [VarT "a", VarT "b"] (VarT "c"), VarT "a"] (ArrT [VarT "b"] (VarT "c"))
      , ArrT [ArrT [VarT "a", VarT "b"] (VarT "c"), VarT "b"] (ArrT [VarT "a"] (VarT "c"))
      , ArrT [VarT "a"] (ListT (VarT "a"))
      , ArrT [StrT] (ListT StrT)
      , ArrT [ListT NumT] (ListT NumT)
      , ArrT [ListT AtomT] (ListT AtomT)
      ]
