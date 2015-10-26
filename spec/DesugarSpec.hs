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
                 (CaseE (VarE "f")
                    [( VarPB "f"
                     , CaseE (VarE "g")
                         [( VarPB "g"
                          , FnE ["x"]
                              (CaseE (VarE "x")
                                 [( VarPB "x"
                                  , AppE (VarE "f") [AppE (VarE "g") [VarE "x"]]
                                  )])
                          )]
                     )]))

    , Def "add" (FnE ["x"]
                   (CaseE (VarE "x")
                      [( VarPB "x"
                       , AppE (VarE "+") [VarE "x",LitE (NumB 1.0)]
                       )]))

    , Def "add2" (AppE (VarE ".") [VarE "add",VarE "add"])
    ]

  desugarFile "test/divmod.geom" $
    let intCast = AppE (VarE "int") [ AppE (VarE "/") [VarE "x", VarE "y"]] in
    [ Def "div" (FnE ["x", "y"]
                   (CaseE (VarE "x")
                      [( VarPB "x"
                       , CaseE (VarE "y")
                           [( VarPB "y"
                            , intCast
                            )]
                       )]))

    , Def "mod" (FnE ["x", "y"]
                   (CaseE (VarE "x")
                      [( VarPB "x"
                       , CaseE (VarE "y")
                           [( VarPB "y"
                            , (AppE (VarE "-") [ VarE "x"
                                               , AppE (VarE "*") [ VarE "y"
                                                                 , intCast
                                                                 ]
                                               ])
                            )]
                       )]))

    , Eval (AppE (VarE "div") [VarE "a", VarE "b"])
    , Eval (AppE (VarE "mod") [VarE "c", VarE "d"])
    ]

  desugarFile "test/monop_fn.geom" $
    [ Eval (AppE (VarE "~") [numB 10.0])
    , Eval (numB (-10.0))
    , Eval (AppE (VarE "~") [numB (-10)])
    ]

  desugarFile "test/neg.geom" $
    [ Eval (AppE (VarE "~") [AppE (VarE "~") [numB 10.0]])
    , Eval (AppE (VarE "+") [ numB 1
                            , AppE (VarE "~") [ AppE (VarE "+") [ VarE "x"
                                                                , numB 1.0
                                                                ]
                                              ]
                            ])
    , Eval (AppE (VarE "+") [ AppE (VarE "+") [ numB 1.0
                                              , AppE (VarE "~") [VarE "x"]
                                              ]
                            , numB 1.0
                            ])
    ]

  desugarFile "test/not.geom" $
    [ Def "not" (FnE ["p"]
                   (CaseE (VarE "p")
                      [( VarPB "q"
                       , IfE (VarE "q")
                           (VarE "false")
                           (VarE "true")
                       )]))

    , Eval (AppE (VarE "not") [VarE "true"])
    , Eval (AppE (VarE "not") [VarE "false"])
    ]

  desugarFile "test/list_comp.geom" $
    map Eval $
      [ AppE (VarE "_mapa") [ FnE ["x","acc"]
                                (CaseE (VarE "x")
                                   [( VarPB "x"
                                    , CaseE (VarE "acc")
                                        [( VarPB "acc"
                                         , LitE (ConsB (VarE "x") (VarE "acc")))])])
                            , AppE (VarE "_range") [VarE "a",VarE "b"]
                            , LitE NilB
                            ]

      , AppE (VarE "_mapa") [ FnE ["as","acc"]
                                (CaseE (VarE "as")
                                   [ ( ValPB (ConsB "b" "bs")
                                     , CaseE (VarE "b")
                                         [ ( VarPB "b"
                                           , CaseE (VarE "bs")
                                               [ ( ValPB (ConsB "c" "cs")
                                                 , CaseE (VarE "c")
                                                     [ ( VarPB "_"
                                                       , CaseE (VarE "cs")
                                                           [ ( ValPB NilB
                                                             , CaseE (VarE "acc")
                                                                 [( VarPB "acc"
                                                                  , IfE (VarE "y")
                                                                      (LitE (ConsB (VarE "b")
                                                                                   (VarE "acc")))
                                                                      (VarE "acc")
                                                                  )]
                                                             )
                                                           , ( VarPB "_", FallThroughE)
                                                           ]
                                                       )
                                                     ]
                                                 )
                                               , ( VarPB "_", FallThroughE)
                                               ]
                                           )
                                         ]
                                     )
                                   , ( VarPB "_"
                                     , CaseE (VarE "acc")
                                         [ ( VarPB "acc"
                                           , VarE "acc"
                                           )
                                         ]
                                     )
                                   ])
                            , VarE "xs"
                            , LitE NilB
                            ]

      , AppE (VarE "_mapa")
          [ FnE ["as","acc"]
              (CaseE (VarE "as")
                 [ ( ValPB (ConsB "b" "bs")
                   , CaseE (VarE "b")
                       [ ( VarPB "b"
                         , CaseE (VarE "bs")
                             [ ( ValPB (ConsB "c" "cs")
                               , CaseE (VarE "c")
                                   [ ( VarPB "_"
                                     , CaseE (VarE "cs")
                                         [ ( ValPB NilB
                                           , CaseE (VarE "acc")
                                               [ ( VarPB "acc"
                                                 , AppE (VarE "_mapa")
                                                     [ FnE ["y","acc"]
                                                         (CaseE (VarE "y")
                                                            [ ( VarPB "y"
                                                              , CaseE (VarE "acc")
                                                                  [ ( VarPB "acc"
                                                                    , LitE (ConsB (LitE (ConsB (VarE "b")
                                                                                               (LitE (ConsB (VarE "y")
                                                                                                            (LitE NilB)))))
                                                                                  (VarE "acc"))
                                                                    )
                                                                  ]
                                                              )
                                                            ])
                                                     , VarE "ys"
                                                     , VarE "acc"
                                                     ]
                                                 )
                                               ]
                                           )
                                         , ( VarPB "_", FallThroughE)
                                         ]
                                     )
                                   ]
                               )
                             , ( VarPB "_", FallThroughE)
                             ]
                         )
                       ]
                   )
                 , ( VarPB "_"
                   , CaseE (VarE "acc")
                       [ ( VarPB "acc", VarE "acc")])
                 ])
          , VarE "xs"
          , LitE NilB
          ]
      ]

  desugarFile "test/empty.geom" $
    [ Def "foo" (FnE [] (IfE (VarE "true") (numB 1) (numB 2)))
    ]
