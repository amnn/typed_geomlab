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
import TyError

runAndFormat :: Show a => (a -> IO ()) -> Alex a -> BS.ByteString -> IO ()
runAndFormat fmt am input = either putStrLn fmt (runAlex input am)

inGreen :: IO () -> IO ()
inGreen io = setSGR [SetColor Foreground Dull Green] >> io >> setSGR []

tc :: FilePath -> BS.ByteString -> IO ()
tc fname input = runAndFormat (mapM_ fmt) tcM input
  where
    tcM                   = typeCheck . map (fmap desugarExpr) <$> parseExpr

    fmt (Def x (Right t)) = inGreen (putStrLn $ x ++ " :: " ++ show t)
    fmt (Def _ (Left e))  = printError fname input e
    fmt (Eval  (Right t)) = inGreen (putStrLn $ show t)
    fmt (Eval  (Left e))  = printError fname input e

processFile :: String -> IO ()
processFile fname = do
  putStrLn ("\n*** Checking " ++ fname ++ "\n")
  input <- BS.readFile fname
  tc fname input

main :: IO ()
main = do
  files <- getArgs
  mapM_ processFile files
