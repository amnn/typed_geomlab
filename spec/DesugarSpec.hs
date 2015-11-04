module DesugarSpec where

import SpecHelper
import Expr
import Literal
import Patt
import Sugar

spec :: Spec
spec = do
  desugarFile "test/compose.geom" $
    [ Def "."    (FnE 2 (FnE 1 (AppE (VarE 3) [AppE (VarE 2) [VarE 1]])))
    , Def "add"  (FnE 1 (AppE (FreeE "+") [VarE 1, numB 1.0]))
    , Def "add2" (AppE (FreeE ".") [FreeE "add", FreeE "add"])
    ]

  desugarFile "test/divmod.geom" $
    let intCast = AppE (FreeE "int") [ AppE (FreeE "/") [VarE 2, VarE 1]] in
    [ Def "div" (FnE 2 intCast)

    , Def "mod" (FnE 2
                   (AppE (FreeE "-") [ VarE 2
                                     , AppE (FreeE "*") [ VarE 1
                                                        , intCast
                                                        ]
                                     ]))

    , Eval (AppE (FreeE "div") [FreeE "a", FreeE "b"])
    , Eval (AppE (FreeE "mod") [FreeE "c", FreeE "d"])
    ]

  desugarFile "test/monop_fn.geom" $
    [ Eval (AppE (FreeE "~") [numB 10.0])
    , Eval (numB (-10.0))
    , Eval (AppE (FreeE "~") [numB (-10)])
    ]

  desugarFile "test/neg.geom" $
    [ Eval (AppE (FreeE "~") [AppE (FreeE "~") [numB 10.0]])
    , Eval (AppE (FreeE "+") [ numB 1
                             , AppE (FreeE "~") [ AppE (FreeE "+") [ FreeE "x"
                                                                   , numB 1.0
                                                                   ]
                                                ]
                             ])
    , Eval (AppE (FreeE "+") [ AppE (FreeE "+") [ numB 1.0
                                                , AppE (FreeE "~") [FreeE "x"]
                                                ]
                             , numB 1.0
                             ])
    ]

  desugarFile "test/not.geom" $
    [ Def "not" (FnE 1
                   (IfE (VarE 1)
                     (FreeE "false")
                     (FreeE "true")))

    , Eval (AppE (FreeE "not") [FreeE "true"])
    , Eval (AppE (FreeE "not") [FreeE "false"])
    ]

  desugarFile "test/list_comp.geom" $
    map Eval $
      [ AppE (FreeE "_mapa") [ FnE 2
                                 (LitE (ConsB (VarE 2) (VarE 1)))
                             , AppE (FreeE "_range") [FreeE "a", FreeE "b"]
                             , LitE NilB
                             ]

      , AppE (FreeE "_mapa") [ FnE 2 -- ["as","acc"]
                                 (CaseE (VarE 2)
                                    [ ( ValPB (ConsB () ()) -- (ConsB "b" "bs")
                                      , CaseE (VarE 1)
                                          [ ( ValPB (ConsB () ()) -- (ConsB "c" "cs")
                                            , CaseE (VarE 1)
                                                [ ( ValPB NilB
                                                  , IfE (FreeE "y")
                                                      (LitE (ConsB (VarE 4) (VarE 5)))
                                                      (VarE 5)
                                                  )
                                                , ( VarPB "_", FallThroughE)
                                                ]
                                            )
                                          , ( VarPB "_", FallThroughE)
                                          ]
                                      )
                                    , ( VarPB "_", VarE 2)
                                    ])
                             , FreeE "xs"
                             , LitE NilB
                             ]

      , AppE (FreeE "_mapa")
          [ FnE 2
              (CaseE (VarE 2)
                 [ ( ValPB (ConsB () ())
                   , CaseE (VarE 1)
                       [ ( ValPB (ConsB () ())
                         , CaseE (VarE 1)
                             [ ( ValPB NilB
                               , AppE (FreeE "_mapa")
                                   [ FnE 2
                                       (LitE (ConsB (LitE (ConsB (VarE 6)
                                                                 (LitE (ConsB (VarE 2)
                                                                              (LitE NilB)))))
                                                    (VarE 1)))
                                   , FreeE "ys"
                                   , VarE 5
                                   ]
                               )
                             , ( VarPB "_", FallThroughE)
                             ]
                         )
                       , ( VarPB "_", FallThroughE)
                       ]
                   )
                 , ( VarPB "_", VarE 2)
                 ])
          , FreeE "xs"
          , LitE NilB
          ]
      ]

  desugarFile "test/empty.geom" $
    [ Def "foo" (FnE 0 (IfE (FreeE "true") (numB 1) (numB 2)))
    ]
