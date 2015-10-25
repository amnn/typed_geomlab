module SpecHelper
       ( module Test.Hspec
       , lexFile
       , parseFile
       , desugarFile
       ) where

import Control.Monad (unless)
import Desugar (desugarExpr)
import Expr
import GLParser (parseExpr)
import Lexer
import Sugar
import Test.Hspec
import Test.HUnit (assertFailure)
import Token

lexFile :: FilePath -> [Token] -> Spec
lexFile = testFile "lexes" (==) scanTokens

parseFile :: FilePath -> [Para Sugar] -> Spec
parseFile = testFile "parses" (==) parse

desugarFile :: FilePath -> [Para Expr] -> Spec
desugarFile = testFile "desugars" cmp desugar
  where
    paraEq (Eval e)  (Eval f)  = e `alphaEq` f
    paraEq (Def x e) (Def y f) = e `alphaEq` f && x == y
    paraEq _         _         = False

    cmp ps qs = all (uncurry paraEq) (zip ps qs)

type Result a = IO (Either String a)

scanTokens :: String -> Result [Token]
scanTokens input = return (runAlex input loop)
  where
    loop = do (L t _ _) <- alexMonadScan
              if t == Eof
              then return []
              else do rest <- loop
                      return (t:rest)

parse :: String -> Result [Para Sugar]
parse input = return (runAlex input parseExpr)

desugar :: FilePath -> Result [Para Expr]
desugar input = return (runAlex input pAndDExpr)
  where
    pAndDExpr = parseExpr >>= mapM (mapM desugarExpr)

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
