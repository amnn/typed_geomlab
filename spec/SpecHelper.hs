{-# LANGUAGE TypeFamilies #-}
module SpecHelper
       ( module Test.Hspec
       , lexFile
       , locLexFile
       , parseFile
       , locParseFile
       , desugarFile
       , locDesugarFile
       , typeCheckFile
       ) where

import Prelude hiding (Foldable)
import Control.Monad (unless)
import Data.Functor.Foldable
import Desugar (desugarExpr)
import Expr
import GLParser (parseExpr)
import Infer (TyError, typeCheck)
import Lexer
import Location
import Sugar
import Test.Hspec
import Test.HUnit (assertFailure)
import Token
import Type (Ty, alphaEq)

lexFile :: FilePath -> [Token] -> Spec
lexFile = testFile "lexes" (==) scanTokens

locLexFile :: FilePath -> [Lexeme] -> Spec
locLexFile = testFile "located lexes" (==) locScanTokens

parseFile :: FilePath -> [Para Sugar] -> Spec
parseFile = testFile "parses" (==) parse

locParseFile :: FilePath -> [Para Sugar] -> Spec
locParseFile = testFile "parses" (==) locParse

desugarFile :: FilePath -> [Para Expr] -> Spec
desugarFile = testFile "desugars" (==) desugar

locDesugarFile :: FilePath -> [Para Expr] -> Spec
locDesugarFile = testFile "desugars" (==) locDesugar

typeCheckFile :: FilePath -> [Para (Either TyError (Ty Id))] -> Spec
typeCheckFile = testFile "type checks" paraEq tc
  where
    paraEq xs ys = and (zipWith eq xs ys)

    eq (Eval e)  (Eval f)  = eitherEq e f
    eq (Def x a) (Def y b) = eitherEq a b && x == y
    eq _         _         = False

    eitherEq (Right u) (Right v) = alphaEq u v
    eitherEq (Left  e) (Left  f) = e == f
    eitherEq _         _         = False

type Result a = IO (Either String a)

scanTokens :: String -> Result [Token]
scanTokens input = return (runAlex input loop)
  where
    loop = do (L _ t) <- alexMonadScan
              if t == Eof
              then return []
              else do rest <- loop
                      return (t:rest)

locScanTokens :: String -> Result [Lexeme]
locScanTokens input = return (runAlex input loop)
  where
    loop = do l@(L _ t) <- alexMonadScan
              if t == Eof
              then return []
              else do rest <- loop
                      return (l:rest)

stripLocS :: Sugar -> Sugar
stripLocS = cata s
  where
    s (LocSB le) = dislocate le
    s e          = embed e

stripLocE :: Expr -> Expr
stripLocE = cata s
  where
    s (LocEB le) = dislocate le
    s e          = embed e

parse :: String -> Result [Para Sugar]
parse input = return (runAlex input pAndSExpr)
  where
    pAndSExpr = map (fmap stripLocS) <$> parseExpr

locParse :: String -> Result [Para Sugar]
locParse input = return (runAlex input parseExpr)

desugar :: String -> Result [Para Expr]
desugar input = return (runAlex input pAndDExpr)
  where
    pAndDExpr = map (fmap (stripLocE . desugarExpr)) <$> parseExpr

locDesugar :: String -> Result [Para Expr]
locDesugar input = return (runAlex input pAndDExpr)
  where
    pAndDExpr = map (fmap desugarExpr) <$> parseExpr

tc :: String -> Result ([Para (Either TyError (Ty Id))])
tc input = return (runAlex input pDAndTCExpr)
  where
    pDAndTCExpr = typeCheck . map (fmap desugarExpr) <$> parseExpr

testFile :: Show a
         => String
         -> (a -> a -> Bool)
         -> (String -> Result a)
         -> FilePath -> a -> Spec

testFile typ cmp process fname expected =
  describe fname $ do
    it typ $ do
      input <- readFile fname
      Right actual <- process input
      let msg = concat ["expected: ", show expected, "\n but got: ", show actual]
      unless (cmp expected actual) (assertFailure msg)
