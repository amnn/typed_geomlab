module Main where

import Control.Monad ()
import Desugar (desugarExpr)
import GLParser (parseExpr)
import Infer (typeCheck)
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
desugar = runAndFormat (mapM_ print) desugarM
  where
    desugarM = do { s <- parseExpr; return (applyDesugar s) }
    applyDesugar = map (fmap desugarExpr)

tc :: String -> IO ()
tc = runAndFormat fmt tcM
  where
    fmt = mapM_ (either (putStrLn . ("Type Error: "++) . show) print)
    tcM = typeCheck . map (fmap desugarExpr) <$> parseExpr

processFile :: String -> IO ()
processFile fname = do
  putStrLn ("\n\nFile: " ++ fname ++ "\n")
  input <- readFile fname
  putStrLn "*** Raw ***\n"
  putStrLn input
  putStrLn "\n*** Desugared  ***\n"
  desugar input
  putStrLn "\n*** Type Checked ***\n"
  tc input

main :: IO ()
main = do
  files <- getArgs
  mapM_ processFile files
