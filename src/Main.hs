module Main where

import Lexer

main :: IO ()
main = do
  input <- getContents
  print (scanTokens input)
