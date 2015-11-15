module ParserSpec where

import Literal
import Patt
import SpecHelper
import Sugar

spec :: Spec
spec = do
  parseFile "test/compose.geom" $
    [ Def "." (FnS [ FnArm "" [VarP "g", VarP "f"]
                       (FnS [ FnArm "" [VarP "x"]
                                (AppS "g" [(AppS "f" [VarS "x"])])
                                Nothing
                            ])
                       Nothing
                   ])
    , Def "add" (FnS [ FnArm "add" [VarP "x"]
                         (AppS "+" [VarS "x", numB 1.0])
                         Nothing
                     ])
    , Def "add2" (AppS "." [VarS "add", VarS "add"])
    ]

  parseFile "test/divmod.geom" $
    let intCast = AppS "int" [ AppS "/" [VarS "x", VarS "y"]] in
    [ Def "div" (FnS [ FnArm "div" [VarP "x", VarP "y"]
                         intCast
                         Nothing
                     ])
    , Def "mod" (FnS [ FnArm "mod" [VarP "x", VarP "y"]
                         (AppS "-" [ VarS "x"
                                   , AppS "*" [ VarS "y"
                                             , intCast
                                             ]
                                   ])
                         Nothing
                     ])
    , Def "a" (numB 10)
    , Def "b" (numB 4)
    , Eval (AppS "div" [VarS "a", VarS "b"])
    , Eval (AppS "mod" [VarS "a", VarS "b"])
    ]

  parseFile "test/monop_fn.geom" $
    [ Eval (AppS "~" [numB 10.0])
    , Eval (LitS (NumB (-10.0)))
    , Eval (AppS "~" [numB (-10)])
    ]

  parseFile "test/neg.geom" $
    [ Def "x" (numB 1)
    , Eval (AppS "~" [AppS "~" [numB 10.0]])
    , Eval (AppS "+" [ numB 1
                     , AppS "~" [ AppS "+" [ VarS "x"
                                           , numB 1.0
                                           ]
                                ]
                     ])
    , Eval (AppS "+" [ AppS "+" [ numB 1.0
                                , AppS "~" [VarS "x"]
                                ]
                     , numB 1.0
                     ])
    ]

  parseFile "test/not.geom" $
    [ Def "not" (FnS [ FnArm "not" [VarP "p"] (VarS "false") (Just (VarS "p"))
                     , FnArm "not" [VarP "_"] (VarS "true")  Nothing
                     ])
    , Eval (AppS "not" [VarS "true"])
    , Eval (AppS "not" [VarS "false"])
    ]

  parseFile "test/list_comp.geom" $
    [ Def "_mapa" (FnS [ FnArm "_mapa" [VarP "f", nilB, VarP "acc"]
                           (VarS "acc")
                           Nothing
                       , FnArm "_mapa" [VarP "f", consB (VarP "x") (VarP "xs"), VarP "acc"]
                           (AppS "f" [VarS "x", AppS "_mapa" [VarS "f", VarS "xs", VarS "acc"]])
                           Nothing
                       ])

    , Def "_range" (FnS [ FnArm "_range" [VarP "a", VarP "b"]
                            (IfS (AppS ">" [VarS "a", VarS "b"])
                                nilB
                                (AppS ":" [ (VarS "a")
                                          , (AppS "_range" [ AppS "+" [VarS "a", numB 1]
                                                           , VarS "b"
                                                           ])
                                          ]))
                            Nothing
                        ])

    , Def "a"  (numB 1)
    , Def "b"  (numB 10)
    , Def "xs" (consB (consB (strB "foo") (consB (strB "bar") nilB)) nilB)
    , Def "ys" (consB (strB "qux") (consB (strB "quux") nilB))
    , Def "y"  (AppS "numeric" [numB 0])

    , Eval (ListCompS (VarS "x")
              [ GenB (VarP "x") (RangeS (VarS "a") (VarS "b")) ])
    , Eval (ListCompS (VarS "x")
              [ GenB (enlist [VarP "_", VarP "x"]) (VarS "xs")
              , FilterB (VarS "y")
              ])
    , Eval (ListCompS (enlist [VarS "y", VarS "x"])
              [ GenB (enlist [VarP "_", VarP "x"]) (VarS "xs")
              , GenB (VarP "y") (VarS "ys")
              ])
    ]

  parseFile "test/empty.geom" $
    [ Def "foo" (FnS [ FnArm "foo" [] (numB 1.0) (Just (VarS "true"))
                     , FnArm "foo" [] (numB 2.0) Nothing
                     ])
    ]

  parseFile "test/folds.geom" $
    [ Def "foldr" (FnS [ FnArm "foldr" [VarP "f", VarP "e", ValP NilB]
                           (VarS "e")
                           Nothing
                       , FnArm "foldr" [VarP "f", VarP "e", consB (VarP "x") (VarP "xs")]
                           (AppS "f" [VarS "x",AppS "foldr" [VarS "f", VarS "e", VarS "xs"]])
                           Nothing
                       ])

    , Def "foldl" (FnS [ FnArm "foldl" [VarP "f", VarP "e", ValP NilB]
                           (VarS "e")
                           Nothing
                       , FnArm "foldl" [VarP "f", VarP "e", consB (VarP "x") (VarP "xs")]
                           (AppS "foldl" [VarS "f", AppS "f" [VarS "e", VarS "x"], VarS "xs"])
                           Nothing
                       ])

    , Def "map" (FnS [ FnArm "map" [VarP "f",VarP "xs"]
                         (LetS "app" (FnS [ FnArm "app" [VarP "x", VarP "ys"]
                                              (AppS ":" [AppS "f" [VarS "x"],VarS "ys"])
                                              Nothing
                                          ])
                            (AppS "foldr" [VarS "app", nilB, VarS "xs"]))
                         Nothing
                     ])

    , Def "filter" (FnS [ FnArm "filter" [VarP "p", VarP "xs"]
                            (LetS "test" (FnS [ FnArm "test" [VarP "x", VarP "ys"]
                                                  (AppS ":" [VarS "x", VarS "ys"])
                                                  (Just (AppS "p" [VarS "x"]))
                                              , FnArm "test" [VarP "_", VarP "ys"]
                                                  (VarS "ys")
                                                  Nothing
                                              ])
                               (AppS "foldr" [VarS "test", nilB,VarS "xs"]))
                            Nothing
                        ])

    , Def "length" (FnS [ FnArm "length" [VarP "xs"]
                            (LetS "plus1" (FnS [ FnArm "plus1" [VarP "x", VarP "_"]
                                                   (AppS "+" [numB 1.0, VarS "x"])
                                                   Nothing
                                               ])
                               (AppS "foldl" [VarS "plus1", numB 0.0, VarS "xs"]))
                            Nothing
                        ])

    , Def "reverse" (FnS [ FnArm "reverse" [VarP "xs"]
                             (LetS "snoc" (FnS [ FnArm "snoc" [VarP "y", VarP "x"]
                                                   (AppS ":" [VarS "x", VarS "y"])
                                                   Nothing
                                               ])
                                (AppS "foldl" [VarS "snoc", nilB, VarS "xs"]))
                             Nothing
                         ])
    ]

  parseFile "test/let_gen.geom" $
    [ Def "id" (FnS [ FnArm "id" [VarP "x"] (VarS "x") Nothing])

    , Eval (LetS "i" (AppS "id" [VarS "id"]) (AppS "i" [numB 1]))

    , Eval (LetS "f" (FnS [ FnArm "f" [VarP "x"] (VarS "x") Nothing])
              (LetS "g" (AppS "f" [VarS "f"])
                 (AppS "g" [atomB "foo"])))
    ]
