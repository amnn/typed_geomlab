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
import Token
import TyError
import Type

data Opt = Opt { showRaw      :: !Bool
               , showSugar    :: !Bool
               , showExpr     :: !Bool
               , showLoc      :: !Bool
               , showType     :: !Bool
               , showRawError :: !Bool
               }

defaultOpts :: Opt
defaultOpts = Opt { showRaw      = False
                  , showSugar    = False
                  , showExpr     = False
                  , showLoc      = False
                  , showType     = True
                  , showRawError = False
                  }

setOpt :: Bool -> Char -> StateT Opt IO ()
setOpt b o = do
  case o of
    'r' -> modify' $ \opt -> opt{showRaw      = b}
    's' -> modify' $ \opt -> opt{showSugar    = b}
    'e' -> modify' $ \opt -> opt{showExpr     = b}
    'l' -> modify' $ \opt -> opt{showLoc      = b}
    't' -> modify' $ \opt -> opt{showType     = b}
    'E' -> modify' $ \opt -> opt{showRawError = b}
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
        inFaint $ putStrLn "Type Checked"
        let types = typeCheck expr
        reportDefer =<< foldM (disp raw) 0 types
  where
    reportDefer d =
        when (d > 0) $
          inFaint . putStrLn $ "Type checking has been deferred for "
                            ++ show d ++ pluralize d " statement"
                            ++ ", due to earlier errors."
    pluralize 1 s = s
    pluralize _ s = s ++ "s"

    err raw d e
      | isDeferral e = return (d+1)
      | otherwise    = liftIO $ printError fname raw e >> return d

    rawErr e = do
      Opt{showRawError} <- get
      when showRawError $ do
        inFaint (putStrLn "Raw Error\n")
        liftIO $ print e >> putStrLn ""

    disp :: BS.ByteString -> Int -> Para (Either TyError (Ty Id)) -> StateT Opt IO Int
    disp _   d (Def x (Right t)) = inGreen (putStrLn $ x ++ " :: " ++ show t) >> return d
    disp _   d (Eval  (Right t)) = inGreen (putStrLn $ show t) >> return d
    disp raw d (Def _ (Left e))  = rawErr e >> err raw d e
    disp raw d (Eval  (Left e))  = rawErr e >> err raw d e

main :: IO ()
main = evalStateT (liftIO getArgs >>= mapM_ processArg) defaultOpts
