module LexerSpec where

import           Data.Location
import           Data.Token
import           SpecHelper

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

    , Define, Ident "a", BinOp "=", Num 10, Semi
    , Define, Ident "b", BinOp "=", Num 4, Semi

    , Ident "a", BinOp "div", Ident "b", Semi
    , Ident "a", BinOp "mod", Ident "b", Semi
    ]

  locLexFile "test/monop_fn.geom" $
    [ L (S (P 1 1) 0 1)   (MonOp "~")
    , L (S (P 1 2) 1 1)   LPar
    , L (S (P 1 3) 2 2)   (Num 10)
    , L (S (P 1 5) 4 1)   RPar
    , L (S (P 1 6) 5 1)   Semi
    , L (S (P 1 8) 7 3)   (Num (-10))
    , L (S (P 1 11) 10 1) Semi
    , L (S (P 1 13) 12 1) (BinOp "-")
    , L (S (P 1 14) 13 1) LPar
    , L (S (P 1 15) 14 3) (Num (-10))
    , L (S (P 1 18) 17 1) RPar
    , L (S (P 1 19) 18 1) Semi
    ]

  locLexFile "test/neg.geom" $
    [ L (S (P 1 1) 0 6)   Define
    , L (S (P 1 8) 7 1)   (Ident "x")
    , L (S (P 1 10) 9 1)  (BinOp "=")
    , L (S (P 1 12) 11 1) (Num 1)
    , L (S (P 1 13) 12 1) Semi
    , L (S (P 3 1) 15 1)  (BinOp "-")
    , L (S (P 3 3) 17 1)  (BinOp "-")
    , L (S (P 3 5) 19 2)  (Num 10)
    , L (S (P 3 7) 21 1)  Semi
    , L (S (P 3 9) 23 1)  (Num 1)
    , L (S (P 3 11) 25 1) (BinOp "+")
    , L (S (P 3 13) 27 1) (BinOp "-")
    , L (S (P 3 15) 29 1) LPar
    , L (S (P 3 16) 30 1) (Ident "x")
    , L (S (P 3 18) 32 1) (BinOp "+")
    , L (S (P 3 20) 34 1) (Num 1)
    , L (S (P 3 21) 35 1) RPar
    , L (S (P 3 22) 36 1) Semi
    , L (S (P 3 24) 38 1) (Num 1)
    , L (S (P 3 26) 40 1) (BinOp "+")
    , L (S (P 3 28) 42 1) (BinOp "-")
    , L (S (P 3 29) 43 1) (Ident "x")
    , L (S (P 3 31) 45 1) (BinOp "+")
    , L (S (P 3 33) 47 1) (Num 1)
    , L (S (P 3 34) 48 1) Semi
    ]

  locLexFile "test/not.geom" $
    [ L (S (P 1 1) 0 6)   Define
    , L (S (P 1 8) 7 3)   (MonOp "not")
    , L (S (P 1 11) 10 1) LPar
    , L (S (P 1 12) 11 1) (Ident "p")
    , L (S (P 1 13) 12 1) RPar
    , L (S (P 1 15) 14 1) (BinOp "=")
    , L (S (P 1 17) 16 5) (Ident "false")
    , L (S (P 1 23) 22 4) When
    , L (S (P 1 28) 27 1) (Ident "p")
    , L (S (P 2 6) 34 1)  VBar
    , L (S (P 2 8) 36 3)  (MonOp "not")
    , L (S (P 2 11) 39 1) LPar
    , L (S (P 2 12) 40 1) Anon
    , L (S (P 2 13) 41 1) RPar
    , L (S (P 2 15) 43 1) (BinOp "=")
    , L (S (P 2 17) 45 4) (Ident "true")
    , L (S (P 2 21) 49 1) Semi
    , L (S (P 4 1) 52 3)  (MonOp "not")
    , L (S (P 4 5) 56 4)  (Ident "true")
    , L (S (P 4 9) 60 1)  Semi
    , L (S (P 5 1) 62 3)  (MonOp "not")
    , L (S (P 5 5) 66 5)  (Ident "false")
    , L (S (P 5 10) 71 1) Semi
    ]

  lexFile "test/list_comp.geom" $
    [ Define, Ident "_mapa"
    , LPar, Ident "f", Comma, Bra, Ket, Comma, Ident "acc", RPar
    , BinOp "=", Ident "acc"
    , VBar, Ident "_mapa"
    , LPar, Ident "f", Comma, Ident "x", BinOp ":", Ident "xs", Comma, Ident "acc", RPar
    , BinOp "=", Ident "f"
    , LPar, Ident "x"
    , Comma, Ident "_mapa", LPar, Ident "f", Comma, Ident "xs", Comma, Ident "acc", RPar, RPar, Semi

    , Define, Ident "_range"
    , LPar, Ident "a", Comma, Ident "b", RPar
    , BinOp "=", If, Ident "a", BinOp ">", Ident "b"
    , Then, Bra, Ket
    , Else, Ident "a", BinOp ":", Ident "_range"
    , LPar, Ident "a", BinOp "+", Num 1
    , Comma, Ident "b", RPar, Semi

    , Define, Ident "a", BinOp "=", Num 1, Semi
    , Define, Ident "b", BinOp "=", Num 10, Semi

    , Define, Ident "xs", BinOp "=", Bra, Bra, Str "foo", Comma, Str "bar", Ket, Ket, Semi
    , Define, Ident "ys", BinOp "=", Bra, Str "qux", Comma, Str "quux", Ket, Semi

    , Define, Ident "y", BinOp "=", Ident "numeric", LPar, Num 0, RPar, Semi

    , Bra, Ident "x", VBar
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

  locLexFile "test/empty.geom" $
    [ L (S (P 1 1) 0 6)   Define
    , L (S (P 1 8) 7 3)   (Ident "foo")
    , L (S (P 1 11) 10 1) LPar
    , L (S (P 1 12) 11 1) RPar
    , L (S (P 1 14) 13 1) (BinOp "=")
    , L (S (P 1 16) 15 1) (Num 1)
    , L (S (P 1 18) 17 4) When
    , L (S (P 1 23) 22 4) (Ident "true")
    , L (S (P 2 6) 32 1)  VBar
    , L (S (P 2 8) 34 3)  (Ident "foo")
    , L (S (P 2 11) 37 1) LPar
    , L (S (P 2 12) 38 1) RPar
    , L (S (P 2 14) 40 1) (BinOp "=")
    , L (S (P 2 16) 42 1) (Num 2)
    , L (S (P 2 17) 43 1) Semi
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
    , Let, Ident "plus1", LPar, Ident "x", Comma, Anon, RPar, BinOp "="
    , Num 1.0, BinOp "+", Ident "x", In
    , Ident "foldl", LPar, Ident "plus1", Comma, Num 0.0, Comma, Ident "xs", RPar, Semi

    , Define
    , Ident "reverse", LPar, Ident "xs", RPar, BinOp "="
    , Let, Ident "snoc", LPar, Ident "y", Comma, Ident "x", RPar, BinOp "="
    , Ident "x", BinOp ":", Ident "y", In
    , Ident "foldl", LPar, Ident "snoc", Comma, Bra, Ket, Comma, Ident "xs", RPar, Semi
    ]

  lexFile "test/let_gen.geom" $
    [ Define, Ident "id", LPar, Ident "x", RPar, BinOp "=", Ident "x", Semi

    , Let, Ident "i"
    , BinOp "=", Ident "id", LPar, Ident "id", RPar
    , In, Ident "i", LPar, Num 1, RPar, Semi

    , Let, Ident "f", LPar, Ident "x", RPar, BinOp "=", Ident "x", In
    , Let, Ident "g", BinOp "=", Ident "f", LPar, Ident "f", RPar, In
    , Ident "g", LPar, Atom "foo", RPar, Semi
    ]

  lexFile "test/section.geom" $
    [ Define, Ident "_lsect", LPar, Ident "f", Comma, Ident "x", RPar, BinOp "="
    , Function, LPar, Ident "y", RPar, Ident "f", LPar, Ident "x", Comma, Ident "y", RPar, Semi

    , Define, Ident "_rsect", LPar, Ident "f", Comma, Ident "y", RPar, BinOp "="
    , Function, LPar, Ident "x", RPar, Ident "f", LPar, Ident "x", Comma, Ident "y", RPar, Semi

    , LPar, BinOp ":", Bra, Ket, RPar, Semi
    , LPar, BinOp ":", Bra, Str "a", Ket, RPar, Semi
    , LPar, Num 1, BinOp ":", RPar, Semi
    , LPar, Atom "foo", BinOp ":", RPar, Semi
    ]
