module Main where

import Control.Monad ()
import Desugar (desugarExpr)
import GLParser (parseExpr)
import Infer (typeCheck)
import Lexer
import Location
import Sugar
import System.Environment
import Token

runAndFormat :: Show a => (a -> IO ()) -> Alex a -> String -> IO ()
runAndFormat fmt am input = either putStrLn fmt (runAlex input am)

scanTokens :: String -> IO ()
scanTokens = runAndFormat (putStrLn . unwords . map show) loop
  where
    loop = do (L _ t) <- alexMonadScan
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
tc = runAndFormat (mapM_ fmt) tcM
  where
    tcM                   = typeCheck . map (fmap desugarExpr) <$> parseExpr
    fmt (Def x (Right t)) = putStrLn $ x ++ " :: " ++ show t
    fmt (Def x (Left t))  = putStrLn $ "Error in definition of \'" ++ x ++ "\':\n" ++ show t
    fmt (Eval et)         = either (putStrLn . ("Error in expression:\n"++) . show) print et

processFile :: String -> IO ()
processFile fname = do
  putStrLn ("\n*** Checking " ++ fname ++ "\n")
  input <- readFile fname
  putStrLn input
  putStrLn " --- \n"
  tc input

main :: IO ()
main = do
  files <- getArgs
  mapM_ processFile files
