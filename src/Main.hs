module Main where

import Control.Monad
import Desugar
import Expr
import GLParser (parseExpr)
import Lexer
import Sugar
import System.Environment
import Token

runAndFormat :: Show a => (a -> IO ()) -> Alex a -> String -> IO ()
runAndFormat fmt am input = either putStrLn fmt (runAlex input am)

scanTokens :: String -> IO ()
scanTokens = runAndFormat (putStrLn . unwords . map (show . stripLoc)) loop
  where
    stripLoc (L t _ _) = t
    loop = do l@(L tok _ _) <- alexMonadScan
              if tok == Eof
              then return []
              else do rest <- loop
                      return (l:rest)


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
