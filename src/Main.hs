module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad ()
import Desugar (desugarExpr)
import GLParser (parseExpr)
import Infer (typeCheck)
import Lexer
import Sugar
import System.Console.ANSI
import System.Environment
import Token
import TyError
import Type

runAndFormat :: Show a => (a -> IO ()) -> Alex a -> BS.ByteString -> IO ()
runAndFormat fmt am input = either putStrLn fmt (runAlex input am)

printTy :: Id -> Ty Id -> IO ()
printTy x t = do
  setSGR [SetColor Foreground Dull Green]
  putStrLn $ x ++ " :: " ++ show t
  setSGR []

tc :: FilePath -> BS.ByteString -> IO ()
tc fname input = runAndFormat (mapM_ fmt) tcM input
  where
    tcM                   = typeCheck . map (fmap desugarExpr) <$> parseExpr
    fmt (Def x (Right t)) = printTy x t

    fmt (Def _ (Left e))  = printError fname input e
    fmt (Eval et)         = either (printError fname input) print et

processFile :: String -> IO ()
processFile fname = do
  putStrLn ("\n*** Checking " ++ fname ++ "\n")
  input <- BS.readFile fname
  tc fname input

main :: IO ()
main = do
  files <- getArgs
  mapM_ processFile files
