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

  lexFile "test/empty.geom" $
    [ Define, Ident "foo", LPar, RPar, BinOp "="
    , Num 1.0, When, Ident "true"
    , VBar, Ident "foo", LPar, RPar, BinOp "=", Num 2.0
    , Semi
    ]

  lexFile "test/folds.geom" $
    [ Define
    , Ident "foldr", LPar , Ident "f", Comma , Ident "e", Comma
    , Bra, Ket, RPar, BinOp "=" , Ident "e", VBar
    , Ident "foldr", LPar , Ident "f", Comma , Ident "e", Comma
    , Ident "x", BinOp ":", Ident "xs", RPar, BinOp "="
    , Ident "f", LPar, Ident "x"
    , Comma, Ident "foldr", LPar, Ident "f", Comma, Ident "e", Comma, Ident "xs", RPar
    , RPar, Semi

    , Define
    , Ident "foldl", LPar , Ident "f", Comma , Ident "e", Comma
    , Bra, Ket, RPar, BinOp "=" , Ident "e", VBar
    , Ident "foldl", LPar , Ident "f", Comma , Ident "e", Comma
    , Ident "x", BinOp ":", Ident "xs", RPar, BinOp "="
    , Ident "foldl", LPar, Ident "f"
    , Comma, Ident "f", LPar, Ident "e", Comma, Ident "x", RPar
    , Comma, Ident "xs", RPar, Semi

    , Define
    , Ident "map", LPar, Ident "f", Comma, Ident "xs", RPar, BinOp "="
    , Let
    , Ident "app", LPar, Ident "x", Comma, Ident "ys", RPar, BinOp "="
    , Ident "f", LPar, Ident "x", RPar, BinOp ":", Ident "ys", In
    , Ident "foldr", LPar, Ident "app", Comma, Bra, Ket, Comma, Ident "xs", RPar, Semi

    , Define
    , Ident "filter", LPar, Ident "p", Comma, Ident "xs", RPar, BinOp "="
    , Let
    , Ident "test", LPar, Ident "x", Comma, Ident "ys", RPar, BinOp "="
    , Ident "x", BinOp ":", Ident "ys", When, Ident "p", LPar, Ident "x", RPar, VBar
    , Ident "test", LPar, Anon, Comma, Ident "ys", RPar, BinOp "="
    , Ident "ys", In
    , Ident "foldr", LPar, Ident "test", Comma, Bra, Ket, Comma, Ident "xs", RPar, Semi

    , Define
    , Ident "length", LPar, Ident "xs", RPar, BinOp "="
    , Let, Ident "plus1", LPar, Anon, Comma, Ident "x", RPar, BinOp "="
    , Num 1.0, BinOp "+", Ident "x", In
    , Ident "foldl", LPar, Ident "plus1", Comma, Num 0.0, Comma, Ident "xs", RPar, Semi

    , Define
    , Ident "reverse", LPar, Ident "xs", RPar, BinOp "="
    , Let, Ident "snoc", LPar, Ident "x", Comma, Ident "y", RPar, BinOp "="
    , Ident "y", BinOp ":", Ident "x", In
    , Ident "foldl", LPar, Ident "snoc", Comma, Bra, Ket, Comma, Ident "xs", RPar, Semi
    ]
