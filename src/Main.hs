module Main where

import Control.Monad
import Desugar
import Expr
import GLParser (parseExpr)
import Lexer
import Sugar
import Token

scanTokens :: String -> [Lexeme]
scanTokens input = either error id (runAlex input loop)
  where
    res = runAlex input loop
    loop = do l@(L tok _ _) <- alexMonadScan
              if tok == Eof
              then return []
              else do rest <- loop
                      return (l:rest)


parse :: String -> [Para Sugar]
parse input = either error id (runAlex input parseExpr)

desugar :: String -> [Para Expr]
desugar input = either error id (runAlex input parseAndDesugar)
  where
    parseAndDesugar = do
      ps <- parseExpr
      mapM (mapM desugarExpr) ps

main :: IO ()
main = do
  input <- getContents
  print (scanTokens input)
  print (parse input)
  print (desugar input)
