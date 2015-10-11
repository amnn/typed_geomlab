module Main where

import Control.Monad ()
import Desugar (desugarExpr)
import GLParser (parseExpr)
import Lexer
import System.Environment
import Token

runAndFormat :: Show a => (a -> IO ()) -> Alex a -> String -> IO ()
runAndFormat fmt am input = either putStrLn fmt (runAlex input am)

scanTokens :: String -> IO ()
scanTokens = runAndFormat (putStrLn . unwords . map show) loop
  where
    loop = do (L t _ _) <- alexMonadScan
              if t == Eof
              then return []
              else do rest <- loop
                      return (t:rest)


parse :: String -> IO ()
parse = runAndFormat (mapM_ print) parseExpr

desugar :: String -> IO ()
desugar = runAndFormat (mapM_ print) (parseExpr >>= mapM (mapM desugarExpr))

processFile :: String -> IO ()
processFile fname = do
  putStrLn ("\n\nFile: " ++ fname ++ "\n")
  input <- readFile fname
  putStrLn "*** Raw ***\n"
  putStrLn input
  putStrLn "*** Scanned  ***\n"
  scanTokens input
  putStrLn "\n*** Parsed ***\n"
  parse input
  putStrLn "\n*** Desugared  ***\n"
  desugar input

main :: IO ()
main = do
  files <- getArgs
  mapM_ processFile files
