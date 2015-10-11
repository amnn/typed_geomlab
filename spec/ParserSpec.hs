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
    , Eval (AppS "div" [VarS "a", VarS "b"])
    , Eval (AppS "mod" [VarS "c", VarS "d"])
    ]

  parseFile "test/monop_fn.geom" $
    [ Eval (AppS "~" [numB 10.0])
    , Eval (LitS (NumB (-10.0)))
    , Eval (AppS "~" [numB (-10)])
    ]

  parseFile "test/neg.geom" $
    [ Eval (AppS "~" [AppS "~" [numB 10.0]])
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
                     , FnArm "not" [AnonP]    (VarS "true")  Nothing
                     ])
    , Eval (AppS "not" [VarS "true"])
    , Eval (AppS "not" [VarS "false"])
    ]

  parseFile "test/list_comp.geom" $
    map Eval $
      [ ListCompS (VarS "x")
          [ GenB (VarP "x") (RangeS (VarS "a") (VarS "b")) ]
      , ListCompS (VarS "x")
          [ GenB (enlist [AnonP, (VarP "x")]) (VarS "xs")
          , FilterB (VarS "y")
          ]
      , ListCompS (enlist [VarS "y", VarS "x"])
          [ GenB (enlist [AnonP, (VarP "x")]) (VarS "xs")
          , GenB (VarP "y") (VarS "ys")
          ]
      ]
