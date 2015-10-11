{-# OPTIONS_GHC -w #-}
module DesugarSpec where

import Expr
import Literal
import Patt
import SpecHelper
import Sugar

spec :: Spec
spec = do
  desugarFile "test/compose.geom" $
    [ Def "." (FnE [ FnArm "" [VarP "g", VarP "f"]
                       (FnE [ FnArm "" [VarP "x"]
                                (AppE (VarE "g") [(AppE (VarE "f") [VarE "x"])])
                                Nothing
                            ])
                       Nothing
                   ])
    , Def "add" (FnE [ FnArm "add" [VarP "x"]
                         (AppE (VarE "+") [VarE "x", numB 1.0])
                         Nothing
                     ])
    , Def "add2" (AppE (VarE ".") [VarE "add", VarE "add"])
    ]

  desugarFile "test/divmod.geom" $
    let intCast = AppE (VarE "int") [ AppE (VarE "/") [VarE "x", VarE "y"]] in
    [ Def "div" (FnE [ FnArm "div" [VarP "x", VarP "y"]
                         intCast
                         Nothing
                     ])
    , Def "mod" (FnE [ FnArm "mod" [VarP "x", VarP "y"]
                         (AppE (VarE "-") [ VarE "x"
                                          , AppE (VarE "*") [ VarE "y"
                                                            , intCast
                                                            ]
                                   ])
                         Nothing
                     ])
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
    [ Def "not" (FnE [ FnArm "not" [VarP "p"] (VarE "false") (Just (VarE "p"))
                     , FnArm "not" [AnonP]    (VarE "true")  Nothing
                     ])
    , Eval (AppE (VarE "not") [VarE "true"])
    , Eval (AppE (VarE "not") [VarE "false"])
    ]
