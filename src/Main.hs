module Main where

import Lexer
import GLParser (parse)

main :: IO ()
main = do
  input <- getContents
  print (scanTokens input)
  print (parse input)
