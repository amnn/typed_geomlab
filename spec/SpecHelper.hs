module SpecHelper
       ( module Test.Hspec
       , lexFile
       , parseFile
       , desugarFile
       , typeCheckFile
       ) where

import Control.Monad (unless)
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

parseFile :: FilePath -> [Para Sugar] -> Spec
parseFile = testFile "parses" (==) parse

desugarFile :: FilePath -> [Para Expr] -> Spec
desugarFile = testFile "desugars" (==) desugar

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

parse :: String -> Result [Para Sugar]
parse input = return (runAlex input parseExpr)

desugar :: String -> Result [Para Expr]
desugar input = return (runAlex input pAndDExpr)
  where
    pAndDExpr = parseExpr >>= return . map (fmap desugarExpr)

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
