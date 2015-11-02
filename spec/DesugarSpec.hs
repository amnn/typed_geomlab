module DesugarSpec where

import SpecHelper
import Expr
import Literal
import Patt
import Sugar

spec :: Spec
spec = do
  desugarFile "test/compose.geom" $
    [ Def "." (FnE ["f","g"]
                 (FnE ["x"]
                    (AppE (FreeE "f") [AppE (FreeE "g") [FreeE "x"]])))

    , Def "add" (FnE ["x"]
                   (AppE (FreeE "+") [FreeE "x", LitE (NumB 1.0)]))

    , Def "add2" (AppE (FreeE ".") [FreeE "add",FreeE "add"])
    ]

  desugarFile "test/divmod.geom" $
    let intCast = AppE (FreeE "int") [ AppE (FreeE "/") [FreeE "x", FreeE "y"]] in
    [ Def "div" (FnE ["x", "y"] intCast)

    , Def "mod" (FnE ["x", "y"]
                   (AppE (FreeE "-") [ FreeE "x"
                                     , AppE (FreeE "*") [ FreeE "y"
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
    [ Def "not" (FnE ["p"]
                   (IfE (FreeE "p")
                     (FreeE "false")
                     (FreeE "true")))

    , Eval (AppE (FreeE "not") [FreeE "true"])
    , Eval (AppE (FreeE "not") [FreeE "false"])
    ]

  desugarFile "test/list_comp.geom" $
    map Eval $
      [ AppE (FreeE "_mapa") [ FnE ["x","acc"]
                                 (LitE (ConsB (FreeE "x") (FreeE "acc")))
                             , AppE (FreeE "_range") [FreeE "a", FreeE "b"]
                             , LitE NilB
                             ]

      , AppE (FreeE "_mapa") [ FnE ["as","acc"]
                                 (CaseE (FreeE "as")
                                    [ ( ValPB (ConsB "b" "bs")
                                      , CaseE (FreeE "bs")
                                          [ ( ValPB (ConsB "c" "cs")
                                            , CaseE (FreeE "cs")
                                                [ ( ValPB NilB
                                                  , IfE (FreeE "y")
                                                      (LitE (ConsB (FreeE "b") (FreeE "acc")))
                                                      (FreeE "acc")
                                                  )
                                                , ( VarPB "_", FallThroughE)
                                                ]
                                            )
                                          , ( VarPB "_", FallThroughE)
                                          ]
                                      )
                                    , ( VarPB "_", FreeE "acc")
                                    ])
                             , FreeE "xs"
                             , LitE NilB
                             ]

      , AppE (FreeE "_mapa")
          [ FnE ["as","acc"]
              (CaseE (FreeE "as")
                 [ ( ValPB (ConsB "b" "bs")
                   , CaseE (FreeE "bs")
                       [ ( ValPB (ConsB "c" "cs")
                         , CaseE (FreeE "cs")
                             [ ( ValPB NilB
                               , AppE (FreeE "_mapa")
                                   [ FnE ["d","acc"]
                                       (LitE (ConsB (LitE (ConsB (FreeE "b")
                                                                 (LitE (ConsB (FreeE "d")
                                                                              (LitE NilB)))))
                                                    (FreeE "acc")))
                                   , FreeE "ys"
                                   , FreeE "acc"
                                   ]
                               )
                             , ( VarPB "_", FallThroughE)
                             ]
                         )
                       , ( VarPB "_", FallThroughE)
                       ]
                   )
                 , ( VarPB "_", FreeE "acc")
                 ])
          , FreeE "xs"
          , LitE NilB
          ]
      ]

  desugarFile "test/empty.geom" $
    [ Def "foo" (FnE [] (IfE (FreeE "true") (numB 1) (numB 2)))
    ]
