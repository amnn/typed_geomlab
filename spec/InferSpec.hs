module InferSpec where

import SpecHelper
import Sugar
import Token
import TyError
import Type

def :: Id -> Ty Id -> Para (Either TyError (Ty Id))
def x = Def x . Right

eval :: Ty Id -> Para (Either TyError (Ty Id))
eval = Eval . Right

spec :: Spec
spec = do
  typeCheckFile "test/compose.geom" $
    [ def "." $ ArrT [ ArrT [VarT "b"] (VarT "c")
                     , ArrT [VarT "a"] (VarT "b")
                     ]
                     ( ArrT [VarT "a"] (VarT "c"))

    , def "add"  $ ArrT [NumT] NumT
    , def "add2" $ ArrT [NumT] NumT
    ]

  typeCheckFile "test/divmod.geom" $
    [ def "div" $ ArrT [NumT, NumT] NumT
    , def "mod" $ ArrT [NumT, NumT] NumT

    , def "a" $ NumT
    , def "b" $ NumT

    , eval $ NumT
    , eval $ NumT
    ]

  typeCheckFile "test/empty.geom" $
    [ def "foo" $ ArrT [] NumT
    ]

  typeCheckFile "test/folds.geom" $
    [ def "foldr" $ ArrT [ArrT [VarT "a", VarT "b"] (VarT "b"), VarT "b", ListT (VarT "a")] (VarT "b")
    , def "foldl" $ ArrT [ArrT [VarT "b", VarT "a"] (VarT "b"), VarT "b", ListT (VarT "a")] (VarT "b")
    , def "map" $ ArrT [ArrT [VarT "a"] (VarT "b"), ListT (VarT "a")] (ListT (VarT "b"))
    , def "filter" $ ArrT [ArrT [VarT "a"] BoolT, ListT (VarT "a")] (ListT (VarT "a"))
    , def "length" $ ArrT [ListT (VarT "a")] NumT
    , def "reverse" $ ArrT [ListT (VarT "a")] (ListT (VarT "a"))
    ]

  typeCheckFile "test/let_gen.geom" $
    [ def "id" $ ArrT [VarT "a"] (VarT "a")
    , eval $ NumT
    , eval $ AtomT
    ]

  typeCheckFile "test/list_comp.geom" $
    [ def "_mapa" $ ArrT [ArrT [VarT "a", VarT "b"] (VarT "b"), ListT (VarT "a"), VarT "b"] (VarT "b")
    , def "_range" $ ArrT [NumT, NumT] (ListT NumT)

    , def "a" $ NumT
    , def "b" $ NumT

    , def "xs" $ ListT (ListT StrT)
    , def "ys" $ ListT (StrT)

    , def "y" BoolT

    , eval $ ListT NumT
    , eval $ ListT StrT
    , eval $ ListT (ListT StrT)
    ]

  typeCheckFile "test/monop_fn.geom" $
    map eval [NumT, NumT, NumT]

  typeCheckFile "test/neg.geom" $
    def "x" NumT : map eval [NumT, NumT, NumT]

  typeCheckFile "test/nest.geom" $
    [eval NumT]

  typeCheckFile "test/not.geom" $
    [ def "not" $ ArrT [BoolT] BoolT
    , eval $ BoolT
    , eval $ BoolT
    ]

  typeCheckFile "test/section.geom" $
    [ def "_lsect" $ ArrT [ArrT [VarT "a", VarT "b"] (VarT "c"), VarT "a"] (ArrT [VarT "b"] (VarT "c"))
    , def "_rsect" $ ArrT [ArrT [VarT "a", VarT "b"] (VarT "c"), VarT "b"] (ArrT [VarT "a"] (VarT "c"))
    , eval $ ArrT [VarT "a"] (ListT (VarT "a"))
    , eval $ ArrT [StrT] (ListT StrT)
    , eval $ ArrT [ListT NumT] (ListT NumT)
    , eval $ ArrT [ListT AtomT] (ListT AtomT)
    ]

  typeCheckFile "test/gen_sym.geom" $
    [ def "labcount" $ RefT NumT
    , def "label" $ ArrT [] NumT
    , eval $ NumT
    ]
