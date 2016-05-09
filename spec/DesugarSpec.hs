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

    , Def "a" (numB 10)
    , Def "b" (numB 4)

    , Eval (AppE (FreeE "div") [FreeE "a", FreeE "b"])
    , Eval (AppE (FreeE "mod") [FreeE "a", FreeE "b"])
    ]

  desugarFile "test/monop_fn.geom" $
    [ Eval (AppE (FreeE "~") [numB 10.0])
    , Eval (numB (-10.0))
    , Eval (AppE (FreeE "~") [numB (-10)])
    ]

  desugarFile "test/neg.geom" $
    [ Def "x" (numB 1)
    , Eval (AppE (FreeE "~") [AppE (FreeE "~") [numB 10.0]])
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
                   (CaseE (VarE 1)
                      [ ( ValPB (BoolB True),  FreeE "false")
                      , ( ValPB (BoolB False), FreeE "true")
                      ]))

    , Eval (AppE (FreeE "not") [FreeE "true"])
    , Eval (AppE (FreeE "not") [FreeE "false"])
    ]

  desugarFile "test/list_comp.geom" $
    [ Def "_mapa" (FnE 3 (CaseE (VarE 2)
                            [ ( ValPB NilB, VarE 1)
                            , ( ValPB (ConsB () ())
                              , AppE (VarE 5) [VarE 2, AppE (FreeE "_mapa") [VarE 5, VarE 1, VarE 3]]
                              )
                            , ( VarPB "_", FailE)
                            ]))

    , Def "_range" (FnE 2 (CaseE (AppE (FreeE ">") [VarE 2,VarE 1])
                             [ ( ValPB (BoolB True), nilB)
                             , ( ValPB (BoolB False)
                               , (AppE (FreeE ":")
                                    [ VarE 2
                                    , AppE (FreeE "_range")
                                       [ AppE (FreeE "+")
                                           [ VarE 2
                                           , LitE (NumB 1.0)
                                           ]
                                       , VarE 1
                                       ]
                                    ])
                               )
                             ]))

    , Def "a" (numB 1.0)
    , Def "b" (numB 10.0)
    , Def "xs" (consB (consB (strB "foo") (consB (strB "bar") nilB)) nilB)
    , Def "ys" (consB (strB "qux") (consB (strB "quux") nilB))
    , Def "y" (AppE (FreeE "numeric") [LitE (NumB 0.0)])

    , Eval (AppE (FreeE "_mapa") [ FnE 2
                                     (consB (VarE 2) (VarE 1))
                                 , AppE (FreeE "_range") [FreeE "a", FreeE "b"]
                                 , nilB
                                 ])

    , Eval (AppE (FreeE "_mapa") [ FnE 2
                                     (CaseE (VarE 2)
                                        [ ( ValPB (ConsB () ())
                                          , CaseE (VarE 1)
                                              [ ( ValPB (ConsB () ())
                                                , CaseE (VarE 1)
                                                    [ ( ValPB NilB
                                                      , CaseE (FreeE "y")
                                                          [ (ValPB (BoolB True), consB (VarE 4) (VarE 5))
                                                          , (ValPB (BoolB False), VarE 5)
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
                                 , nilB
                                 ])

    , Eval (AppE (FreeE "_mapa")
              [ FnE 2
                  (CaseE (VarE 2)
                     [ ( ValPB (ConsB () ())
                       , CaseE (VarE 1)
                           [ ( ValPB (ConsB () ())
                             , CaseE (VarE 1)
                                 [ ( ValPB NilB
                                   , AppE (FreeE "_mapa")
                                       [ FnE 2
                                           (consB (consB (VarE 6) (consB (VarE 2) nilB))
                                                  (VarE 1))
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
              , nilB
              ])
    ]

  desugarFile "test/empty.geom" $
    [ Def "foo" (FnE 0 (CaseE (FreeE "true")
                          [ ( ValPB (BoolB True),  numB 1)
                          , ( ValPB (BoolB False), numB 2)
                          ]))
    ]

  desugarFile "test/folds.geom" $
    [ Def "foldr" (FnE 3
                     (CaseE (VarE 1)
                        [ ( ValPB NilB, VarE 2)
                        , ( ValPB (ConsB () ())
                          , AppE (VarE 5) [VarE 2, AppE (FreeE "foldr") [VarE 5, VarE 4, VarE 1]]
                          )
                        , (VarPB "_", FailE)
                        ]))

    , Def "foldl" (FnE 3
                     (CaseE (VarE 1)
                       [ ( ValPB NilB, VarE 2)
                       , ( ValPB (ConsB () ())
                         , AppE (FreeE "foldl") [VarE 5, AppE (VarE 5) [VarE 4, VarE 2], VarE 1]
                         )
                       , (VarPB "_",FailE)
                       ]))

    , Def "map" (FnE 2
                   (LetE
                      (FnE 2
                         (AppE (FreeE ":") [AppE (VarE 5) [VarE 2], VarE 1]))
                      (AppE (FreeE "foldr") [VarE 1, nilB, VarE 2])))

    , Def "filter" (FnE 2
                      (LetE
                         (FnE 2
                            (CaseE (AppE (VarE 5) [VarE 2])
                               [ ( ValPB (BoolB True)
                                 , AppE (FreeE ":") [VarE 2, VarE 1]
                                 )
                               , (ValPB (BoolB False), VarE 1)
                               ]))
                         (AppE (FreeE "foldr") [VarE 1, nilB, VarE 2])))

    , Def "length" (FnE 1
                      (LetE
                        (FnE 2 (AppE (FreeE "+") [numB 1.0, VarE 2]))
                        (AppE (FreeE "foldl") [VarE 1, numB 0.0, VarE 2])))

    , Def "reverse" (FnE 1
                       (LetE
                         (FnE 2 (AppE (FreeE ":") [VarE 1, VarE 2]))
                         (AppE (FreeE "foldl") [VarE 1, nilB, VarE 2])))
    ]

  desugarFile "test/let_gen.geom" $
    [ Def "id" (FnE 1 (VarE 1))

    , Eval (LetE (AppE (FreeE "id") [FreeE "id"])
              (AppE (VarE 1) [numB 1]))

    , Eval (LetE (FnE 1 (VarE 1))
              (LetE (AppE (VarE 2) [VarE 2])
                 (AppE (VarE 1) [atomB "foo"])))
    ]

  desugarFile "test/section.geom" $
    [ Def "_lsect" (FnE 2 (FnE 1 (AppE (VarE 3) [VarE 2, VarE 1])))
    , Def "_rsect" (FnE 2 (FnE 1 (AppE (VarE 3) [VarE 1, VarE 2])))

    , Eval (AppE (FreeE "_rsect") [FreeE ":", nilB])
    , Eval (AppE (FreeE "_rsect") [FreeE ":", consB (strB "a") nilB])
    , Eval (AppE (FreeE "_lsect") [FreeE ":", numB 1])
    , Eval (AppE (FreeE "_lsect") [FreeE ":", atomB "foo"])
    ]
