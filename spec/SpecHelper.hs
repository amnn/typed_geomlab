module SpecHelper
       ( module Test.Hspec
       , lexFile
       , parseFile
       , desugarFile
       ) where

import Desugar (desugarExpr)
import Expr
import GLParser (parseExpr)
import Lexer
import Sugar
import Test.Hspec
import Token

lexFile :: FilePath -> [Token] -> Spec
lexFile = testFile "lexes" scanTokens

parseFile :: FilePath -> [Para Sugar] -> Spec
parseFile = testFile "parses" parse

desugarFile :: FilePath -> [Para Expr] -> Spec
desugarFile = testFile "desugars" desugar

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

testFile :: (Eq a, Show a)
         => String -> (String -> Result a)
         -> FilePath -> a -> Spec

testFile typ process fname expected =
  describe fname $ do
    it typ $ do
      input <- readFile fname
      Right actual <- process input
      actual `shouldBe` expected
