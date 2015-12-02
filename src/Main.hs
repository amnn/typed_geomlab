{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad.State
import Desugar (desugarExpr)
import qualified Expr as E (stripLoc)
import GLParser (parseExpr)
import Infer (typeCheck)
import Lexer
import Sugar hiding (stripLoc)
import qualified Sugar as S (stripLoc)
import System.Console.ANSI
import System.Environment
import TyError

data Opt = Opt { showRaw   :: !Bool
               , showSugar :: !Bool
               , showExpr  :: !Bool
               , showLoc   :: !Bool
               , showType  :: !Bool
               }

defaultOpts :: Opt
defaultOpts = Opt { showRaw   = False
                  , showSugar = False
                  , showExpr  = False
                  , showLoc   = False
                  , showType  = True
                  }

setOpt :: Bool -> Char -> StateT Opt IO ()
setOpt b o = do
  case o of
    'r' -> modify' $ \opt -> opt{showRaw   = b}
    's' -> modify' $ \opt -> opt{showSugar = b}
    'e' -> modify' $ \opt -> opt{showExpr  = b}
    'l' -> modify' $ \opt -> opt{showLoc   = b}
    't' -> modify' $ \opt -> opt{showType  = b}
    _   -> return ()

fmt :: [SGR] -> IO () -> StateT Opt IO ()
fmt sgr io = liftIO (setSGR sgr >> io >> setSGR [])

inRed, inGreen, inBold, inFaint :: IO () -> StateT Opt IO ()
inRed   = fmt [SetColor Foreground Dull Red]
inGreen = fmt [SetColor Foreground Dull Green]
inBold  = fmt [SetConsoleIntensity BoldIntensity]
inFaint = fmt [SetConsoleIntensity FaintIntensity]

processArg :: String -> StateT Opt IO ()

processArg ('+':opts) = mapM_ (setOpt True)  opts
processArg ('-':opts) = mapM_ (setOpt False) opts

processArg fname = do
  inBold $ putStrLn ("\n" ++ fname ++ "\n")

  Opt{showRaw, showSugar, showExpr, showLoc, showType} <- get

  -- Read File
  raw <- liftIO $ BS.readFile fname
  when showRaw $ do
    inFaint $ putStrLn "File Contents\n"
    liftIO $ BS.putStrLn raw >> putStrLn ""

  -- Parsing
  case runAlex raw parseExpr of
    (Left e)      -> inRed $ putStrLn e
    (Right sugar) -> do
      when showSugar $ do
        inFaint $ putStrLn "Parsed\n"
        liftIO $ if showLoc
        then mapM_ print sugar >> putStrLn ""
        else mapM_ (print . fmap S.stripLoc) sugar >> putStrLn ""

      -- Desugaring
      let expr  = map (fmap desugarExpr) sugar
      when showExpr $ do
        inFaint $ putStrLn "Desugared\n"
        liftIO $ if showLoc
        then mapM_ print expr >> putStrLn ""
        else mapM_ (print . fmap E.stripLoc) expr >> putStrLn ""

      -- Type checking
      when showType $ do
        inFaint $ putStrLn "Type Checked\n"
        let types = typeCheck expr
        mapM_ (disp raw) types
  where
    disp _   (Def x (Right t)) = inGreen (putStrLn $ x ++ " :: " ++ show t)
    disp raw (Def _ (Left e))  = liftIO $ printError fname raw e
    disp _   (Eval  (Right t)) = inGreen (putStrLn $ show t)
    disp raw (Eval  (Left e))  = liftIO $ printError fname raw e

main :: IO ()
main = evalStateT (liftIO getArgs >>= mapM_ processArg) defaultOpts
