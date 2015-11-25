module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad ()
import Desugar (desugarExpr)
import GLParser (parseExpr)
import Infer (typeCheck)
import Lexer
import Sugar
import System.Environment

runAndFormat :: Show a => (a -> IO ()) -> Alex a -> BS.ByteString -> IO ()
runAndFormat fmt am input = either putStrLn fmt (runAlex input am)

tc :: BS.ByteString -> IO ()
tc = runAndFormat (mapM_ fmt) tcM
  where
    tcM                   = typeCheck . map (fmap desugarExpr) <$> parseExpr
    fmt (Def x (Right t)) = putStrLn $ x ++ " :: " ++ show t
    fmt (Def x (Left t))  = putStrLn $ "Error in definition of \'" ++ x ++ "\':\n" ++ show t
    fmt (Eval et)         = either (putStrLn . ("Error in expression:\n"++) . show) print et

processFile :: String -> IO ()
processFile fname = do
  putStrLn ("\n*** Checking " ++ fname ++ "\n")
  input <- BS.readFile fname
  BS.putStrLn input
  putStrLn " --- \n"
  tc input

main :: IO ()
main = do
  files <- getArgs
  mapM_ processFile files
