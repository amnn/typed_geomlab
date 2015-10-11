module LexerSpec where

import SpecHelper
import Token

spec :: Spec
spec = do
  lexFile "test/compose.geom" $
    [ Define, BinOp ".", BinOp "="
    , Function, LPar, Ident "g", Comma, Ident "f", RPar
    , Function, LPar, Ident "x", RPar
    , Ident "g", LPar, Ident "f", LPar, Ident "x", RPar, RPar
    , Semi

    , Define, Ident "add", LPar, Ident "x", RPar, BinOp "="
    , Ident "x", BinOp "+", Num 1.0
    , Semi

    , Define, Ident "add2", BinOp "="
    , Ident "add", BinOp ".", Ident "add"
    , Semi
    ]

  lexFile "test/divmod.geom" $
    [ Define, BinOp "div", LPar, Ident "x", Comma, Ident "y", RPar, BinOp "="
    , Ident "int", LPar, Ident "x", BinOp "/", Ident "y", RPar
    , Semi

    , Define, BinOp "mod", LPar, Ident "x", Comma, Ident "y", RPar, BinOp "="
    , Ident "x", BinOp "-"
    , Ident "y", BinOp "*"
    , Ident "int", LPar, Ident "x", BinOp "/", Ident "y", RPar
    , Semi

    , Ident "a", BinOp "div", Ident "b", Semi
    , Ident "c", BinOp "mod", Ident "d", Semi
    ]

  lexFile "test/monop_fn.geom" $
    [ MonOp "~", LPar, Num 10.0, RPar, Semi
    , Num (-10.0), Semi
    , BinOp "-", LPar, Num (-10.0), RPar, Semi
    ]

  lexFile "test/neg.geom" $
    [ BinOp "-", BinOp "-", Num 10.0, Semi
    , Num 1.0, BinOp "+", BinOp "-"
    , LPar, Ident "x", BinOp "+", Num 1.0, RPar, Semi
    , Num 1.0, BinOp "+", BinOp "-", Ident "x", BinOp "+", Num 1.0, Semi
    ]

  lexFile "test/not.geom" $
    [ Define
    , MonOp "not", LPar, Ident "p", RPar, BinOp "="
    , Ident "false", When, Ident "p"
    , VBar
    , MonOp "not", LPar, Anon, RPar, BinOp "="
    , Ident "true"
    , Semi

    , MonOp "not", Ident "true", Semi
    , MonOp "not", Ident "false", Semi
    ]

  lexFile "test/list_comp.geom" $
    [ Bra, Ident "x", VBar
    , Ident "x", Gen
    , Bra, Ident "a", Range, Ident "b", Ket, Ket
    , Semi

    , Bra, Ident "x", VBar
    , Bra, Ident "x", Comma, Anon, Ket, Gen
    , Ident "xs", When, Ident "y", Ket
    , Semi

    , Bra, Bra, Ident "x", Comma, Ident "y", Ket, VBar
    , Bra, Ident "x", Comma, Anon, Ket, Gen, Ident "xs", Comma
    , Ident "y", Gen, Ident "ys", Ket
    , Semi
    ]
