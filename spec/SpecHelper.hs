module SpecHelper
       ( module Test.Hspec
       , scanTokens
       , parse
       , desugar
       ) where

import Desugar (desugarExpr)
import Expr
import GLParser (parseExpr)
import Lexer
import Sugar
import Test.Hspec
import Token

runFromFile :: (String -> IO a) -> FilePath -> IO a
runFromFile f fname = readFile fname >>= f

type Result a = IO (Either String a)

scanTokens :: FilePath -> Result [Token]
scanTokens = runFromFile $ \input -> return (runAlex input loop)
  where
    loop = do (L t _ _) <- alexMonadScan
              if t == Eof
              then return []
              else do rest <- loop
                      return (t:rest)

parse :: FilePath -> Result [Para Sugar]
parse = runFromFile $ \input -> return (runAlex input parseExpr)

desugar :: FilePath -> Result [Para Expr]
desugar = runFromFile $ \input -> return (runAlex input pAndDExpr)
  where
    pAndDExpr = parseExpr >>= mapM (mapM desugarExpr)
