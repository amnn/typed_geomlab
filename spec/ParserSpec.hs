module ParserSpec where

import Literal
import Location
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

  locParseFile "test/monop_fn.geom" $
    [ Eval (annL (LocS "expression") (S (P 1 1) 0 5)
        (AppS "~" [(annL (LocS "1st argument") (S (P 1 2) 1 4) (numB 10.0))]))

    , Eval (annL (LocS "expression") (S (P 1 8) 7 3)
        (LitS (NumB (-10.0))))

    , Eval (annL (LocS "expression") (S (P 1 13) 12 6)
        (AppS "~" [(annL (LocS "1st argument") (S (P 1 14) 13 5) (numB (-10)))]))
    ]

  locParseFile "test/neg.geom" $
    [ Def "x" (annL (LocS "definition of 'x'") (S (P 1 12) 11 1) (numB 1))

    , Eval (annL (LocS "expression") (S (P 3 1) 15 6)
              (AppS "~" [annL (LocS "1st argument") (S (P 3 3) 17 4)
                           (AppS "~" [annL (LocS "1st argument") (S (P 3 5) 19 2)
                                        (numB 10.0)])]))

    , Eval
        (annL (LocS "expression") (S (P 3 9) 23 13)
           (AppS "+" [ annL (LocS "1st argument") (S (P 3 9) 23 1) (numB 1)
                     , annL (LocS "2nd argument") (S (P 3 13) 27 9)
                         (AppS "~" [ annL (LocS "1st argument") (S (P 3 15) 29 7)
                                       (AppS "+" [ annL (LocS "1st argument") (S (P 3 16) 30 1)
                                                     (VarS "x")
                                                 , annL (LocS "2nd argument") (S (P 3 20) 34 1)
                                                     (numB 1.0)
                                                 ])
                                   ])
                     ]))

    , Eval
        (annL (LocS "expression") (S (P 3 24) 38 10)
           (AppS "+" [ annL (LocS "1st argument") (S (P 3 24) 38 6)
                         (AppS "+" [ annL (LocS "1st argument") (S (P 3 24) 38 1) (numB 1.0)
                                   , annL (LocS "2nd argument") (S (P 3 28) 42 2)
                                       (AppS "~" [annL (LocS "1st argument") (S (P 3 29) 43 1)
                                                    (VarS "x")])
                                   ])
                     , annL (LocS "2nd argument") (S (P 3 33) 47 1) (numB 1.0)
                     ]))
    ]

  locParseFile "test/not.geom" $
    [ Def "not" (annL (LocS "definition of 'not'") (S (P 1 8) 7 42)
                   (FnS [ FnArm "not" [VarP "p"]
                            (annL (LocS "body of 'not'") (S (P 1 17) 16 5) (VarS "false"))
                            (Just (annL (LocS "guard") (S (P 1 28) 27 1) (VarS "p")))
                        , FnArm "not" [VarP "_"]
                            (annL (LocS "body of 'not'") (S (P 2 17) 45 4) (VarS "true"))
                            Nothing
                        ]))
    , Eval (annL (LocS "expression") (S (P 4 1) 52 8)
              (AppS "not" [annL (LocS "1st argument") (S (P 4 5) 56 4) (VarS "true")]))

    , Eval (annL (LocS "expression") (S (P 5 1) 62 9)
              (AppS "not" [annL (LocS "1st argument") (S (P 5 5) 66 5) (VarS "false")]))
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

  locParseFile "test/src_map_list_comp.geom" $
    [ Eval (annL (LocS "expression") (S (P 1 1) 0 18)
              (ListCompS (annL (LocS "yield") (S (P 1 3) 2 1) (VarS "x"))
                 [ GenB (VarP "x")
                        (annL (LocS "generator") (S (P 1 12) 11 6)
                           (RangeS (annL (LocS "lowerbound") (S (P 1 13) 12 1) (VarS "a"))
                                   (annL (LocS "upperbound") (S (P 1 16) 15 1) (VarS "b"))))
                 ]))

    , Eval (annL (LocS "expression") (S (P 2 1) 20 26)
              (ListCompS (annL (LocS "yield") (S (P 2 3) 22 1) (VarS "x"))
                [ GenB (enlist [VarP "_", VarP "x"])
                       (annL (LocS "generator") (S (P 2 17) 36 2) (VarS "xs"))
                , FilterB (annL (LocS "guard") (S (P 2 25) 44 1) (VarS "y"))
                ]))

    , Eval (annL (LocS "expression") (S (P 3 1) 48 33)
              (ListCompS (annL (LocS "yield") (S (P 3 3) 50 6)
                            (enlist [ annL (LocS "2nd element") (S (P 3 7) 54 1) (VarS "y")
                                    , annL (LocS "1st element") (S (P 3 4) 51 1) (VarS "x")
                                    ]))
                [ GenB (enlist [VarP "_", VarP "x"])
                       (annL (LocS "generator") (S (P 3 22) 69 2) (VarS "xs"))
                , GenB (VarP "y")
                       (annL (LocS "generator") (S (P 3 31) 78 2) (VarS "ys"))
                ]))
    ]

  locParseFile "test/empty.geom" $
    [ Def "foo" (annL (LocS "definition of 'foo'") (S (P 1 8) 7 36)
                   (FnS [ FnArm "foo" []
                            (annL (LocS "body of 'foo'") (S (P 1 16) 15 1) (numB 1.0))
                            (Just (annL (LocS "guard") (S (P 1 23) 22 4) (VarS "true")))
                        , FnArm "foo" []
                            (annL (LocS "body of 'foo'") (S (P 2 16) 42 1) (numB 2.0))
                            Nothing
                        ]))
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

  parseFile "test/section.geom" $
    [ Def "_lsect" (FnS [ FnArm "_lsect" [VarP "f", VarP "x"]
                            (FnS [ FnArm "" [VarP "y"]
                                     (AppS "f" [VarS "x", VarS "y"])
                                     Nothing
                                 ])
                            Nothing
                        ])

    , Def "_rsect" (FnS [ FnArm "_rsect" [VarP "f", VarP "y"]
                            (FnS [ FnArm "" [VarP "x"]
                                     (AppS "f" [VarS "x", VarS "y"])
                                     Nothing
                                 ])
                            Nothing
                        ])

    , Eval (RSectS ":" nilB)
    , Eval (RSectS ":" (consB (strB "a") nilB))
    , Eval (LSectS (numB 1) ":")
    , Eval (LSectS (atomB "foo") ":")
    ]

  locParseFile "test/src_map_section.geom" $
    [ Eval (annL (LocS "expression") (S (P 1 1) 0 5)
        (RSectS ":" (annL (LocS "right section") (S (P 1 3) 2 2) nilB)))

    , Eval (annL (LocS "expression") (S (P 1 8) 7 8)
        (RSectS ":" (annL (LocS "right section") (S (P 1 10) 9 5)
                       (consB (annL (LocS "1st element") (S (P 1 11) 10 3)
                                 (strB "a"))
                              nilB))))

    , Eval (annL (LocS "expression") (S (P 1 18) 17 4)
              (LSectS (annL (LocS "left section") (S (P 1 19) 18 1) (numB 1)) ":"))

    , Eval (annL (LocS "expression") (S (P 1 24) 23 7)
              (LSectS (annL (LocS "left section") (S (P 1 25) 24 4) (atomB "foo")) ":"))
    ]

  parseFile "test/gen_sym.geom" $
    [ Def "labcount" (AppS "_new" [numB 0])
    , Def "label"    (FnS [ FnArm "label" []
                              (AppS "_set" [ VarS "labcount"
                                           , AppS "+" [ AppS "_get" [VarS "labcount"]
                                                      , numB 1
                                                      ]
                                           ])
                              Nothing
                          ])
    , Eval (AppS "label" [])
    ]
