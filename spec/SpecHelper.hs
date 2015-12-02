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
       , annL
       ) where

import Prelude hiding (Foldable)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad (unless)
import Desugar (desugarExpr)
import Expr hiding (stripLoc)
import qualified Expr as E (stripLoc)
import GLParser (parseExpr)
import Infer (typeCheck)
import Lexer
import Location
import Sugar hiding (stripLoc)
import qualified Sugar as S (stripLoc)
import Test.Hspec
import Test.HUnit (assertFailure)
import Token
import TyError
import Type (Ty, alphaEq)

lexFile :: FilePath -> [Token] -> Spec
lexFile = testFile "lexes" (==) scanTokens

locLexFile :: FilePath -> [Lexeme] -> Spec
locLexFile = testFile "located lexes" (==) locScanTokens

parseFile :: FilePath -> [Para Sugar] -> Spec
parseFile = testFile "parses" (==) parse

locParseFile :: FilePath -> [Para Sugar] -> Spec
locParseFile = testFile "located parses" (==) locParse

desugarFile :: FilePath -> [Para Expr] -> Spec
desugarFile = testFile "desugars" (==) desugar

locDesugarFile :: FilePath -> [Para Expr] -> Spec
locDesugarFile = testFile "located desugars" (==) locDesugar

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

annL :: (Located a -> b) -> Span -> a -> b
annL f s = f . L s

type Result a = IO (Either String a)

scanTokens :: BS.ByteString -> Result [Token]
scanTokens input = return (runAlex input loop)
  where
    loop = do (L _ t) <- alexMonadScan
              if t == Eof
              then return []
              else do rest <- loop
                      return (t:rest)

locScanTokens :: BS.ByteString -> Result [Lexeme]
locScanTokens input = return (runAlex input loop)
  where
    loop = do l@(L _ t) <- alexMonadScan
              if t == Eof
              then return []
              else do rest <- loop
                      return (l:rest)

parse :: BS.ByteString -> Result [Para Sugar]
parse input = return (runAlex input pAndSExpr)
  where
    pAndSExpr = map (fmap S.stripLoc) <$> parseExpr

locParse :: BS.ByteString -> Result [Para Sugar]
locParse input = return (runAlex input parseExpr)

desugar :: BS.ByteString -> Result [Para Expr]
desugar input = return (runAlex input pAndDExpr)
  where
    pAndDExpr = map (fmap (E.stripLoc . desugarExpr)) <$> parseExpr

locDesugar :: BS.ByteString -> Result [Para Expr]
locDesugar input = return (runAlex input pAndDExpr)
  where
    pAndDExpr = map (fmap desugarExpr) <$> parseExpr

tc :: BS.ByteString -> Result ([Para (Either TyError (Ty Id))])
tc input = return (runAlex input pDAndTCExpr)
  where
    pDAndTCExpr = typeCheck . map (fmap desugarExpr) <$> parseExpr

testFile :: Show a
         => String
         -> (a -> a -> Bool)
         -> (BS.ByteString -> Result a)
         -> FilePath -> a -> Spec

testFile typ cmp process fname expected =
  describe fname $ do
    it typ $ do
      input <- BS.readFile fname
      Right actual <- process input
      let msg = concat ["expected: ", show expected, "\n but got: ", show actual]
      unless (cmp expected actual) (assertFailure msg)
