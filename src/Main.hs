module Main where

import Desugar
import GLParser (parse)
import Lexer


main :: IO ()
main = do
  input <- getContents
  print (scanTokens input)
  print (parse input)
  print (map (desugar <$>) (parse input))
