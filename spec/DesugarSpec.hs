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
                                 (consB (VarE 2) (VarE 1))
                             , AppE (FreeE "_range") [FreeE "a", FreeE "b"]
                             , nilB
                             ]

      , AppE (FreeE "_mapa") [ FnE 2
                                 (CaseE (VarE 2)
                                    [ ( ValPB (ConsB () ())
                                      , CaseE (VarE 1)
                                          [ ( ValPB (ConsB () ())
                                            , CaseE (VarE 1)
                                                [ ( ValPB NilB
                                                  , IfE (FreeE "y")
                                                      (consB (VarE 4) (VarE 5))
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
                             , nilB
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
          ]
      ]

  desugarFile "test/empty.geom" $
    [ Def "foo" (FnE 0 (IfE (FreeE "true") (numB 1) (numB 2)))
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
                            (IfE (AppE (VarE 5) [VarE 2])
                               (AppE (FreeE ":") [VarE 2, VarE 1])
                               (VarE 1)))
                         (AppE (FreeE "foldr") [VarE 1, nilB, VarE 2])))

    , Def "length" (FnE 1
                      (LetE
                        (FnE 2 (AppE (FreeE "+") [numB 1.0, VarE 1]))
                        (AppE (FreeE "foldl") [VarE 1, numB 0.0, VarE 2])))

    , Def "reverse" (FnE 1
                       (LetE
                         (FnE 2 (AppE (FreeE ":") [VarE 1, VarE 2]))
                         (AppE (FreeE "foldl") [VarE 1, nilB, VarE 2])))
    ]
