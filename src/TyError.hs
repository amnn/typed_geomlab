module TyError where

import qualified Data.ByteString.Lazy.Char8 as BS
import Location
import System.Console.ANSI
import Token

-- | Possible Type Errors that the type checker may emit
data TyError = UnboundVarE Id
            -- ^ A free variable was found for which no type exists
            -- (i.e. undefined variable).
             | UnificationE
            -- ^ Could not unify two types.
             | OccursE
            -- ^ When unifying a variable with a type, we found the variable
            -- within the type (A cycle was detected in the occurs check).
             | CtxE String (Located TyError)
            -- ^ Adds context to an error in the form of a label and a location.
               deriving (Eq, Show)

printError :: FilePath -> BS.ByteString -> TyError -> IO ()
printError fname input (CtxE root chain@(L rootSp _)) = do
  newLine
  setSGR [ SetUnderlining SingleUnderline
         , SetColor Foreground Vivid Red
         , SetConsoleIntensity BoldIntensity
         ]
  errHead rootSp; putStr "Error in the "; putStrLn root
  setSGR []; newLine
  unwind chain
  newLine

  where
    errHead sp | (P l c) <- start sp =
      mapM_ putStr [fname, ":", show l, ":", show c, ": "]

    snip (S _ o w) = mapM_ indentBS . BS.lines $ extract o w
    snip _         = error "snip: No offset!"

    nest (S _ o1 w) (S _ o2 w2) =
      let w1 = o2 - o1
          o3 = o2 + w2
          w3 = o1 + w - o3
          prologue = extract o1 w1
          focus    = extract o2 w2
          epilogue = extract o3 w3
      in do
        indentMultiFstBS prologue
        setSGR [SetColor Foreground Vivid Blue]
        indentMultiBS focus
        setSGR []
        indentMultiBS epilogue
        newLine

    nest _ _ = error "nest: no offset!"

    indentMultiFstBS msg =
      let nl              = '\n' == BS.last msg
          go []           = []
          go [l] | not nl = [putStr "    " >> BS.putStr l]
          go (l:ls)       = indentBS l : go ls
      in sequence_ . go . BS.lines $ msg

    indentMultiBS msg =
      let nl               = '\n' == BS.last msg
          go []            = []
          go [l]  | not nl = [BS.putStr l]
          go (l:ls)        = BS.putStrLn l : go' ls
          go' []           = []
          go' [l] | not nl = [putStr "    " >> BS.putStr l]
          go' (l:ls)       = indentBS l : go' ls
      in sequence_ . go . BS.lines $ msg

    extract o w = BS.take (fromIntegral w)
                . BS.drop (fromIntegral o)
                $ input

    newLine = putStrLn ""

    indentS  msg = putStr "    " >> putStrLn msg
    indentBS msg = putStr "    " >> BS.putStrLn msg

    unwind (L _ (UnboundVarE x)) = indentS $ "unbound variable '" ++ x ++ "'"
    unwind (L _ UnificationE)    = indentS "unification error"
    unwind (L _ OccursE)         = indentS "cyclicity error"

    unwind (L sp (CtxE lbl (L sp' (CtxE lbl' le@(L sp'' _)))))
      | sp == sp' = do
      unwind le
      newLine; setSGR [SetColor Foreground Dull Red]
      errHead sp'; mapM_ putStr ["In the ", lbl', " of the "]; putStrLn lbl
      setSGR []; newLine
      nest sp' sp''

    unwind (L sp (CtxE _ le@(L sp' _))) | sp == sp' = unwind le

    unwind (L _  (CtxE lbl le@(L sp _))) = do
      unwind le
      newLine; setSGR [SetColor Foreground Dull Red]
      errHead sp; putStr "In the "; putStrLn lbl
      setSGR []; newLine
      snip sp

printError _ _ e = do
  putStrLn "*** INTERNAL ERROR ***"
  putStrLn "An error was returned without a root location"
  putStrLn "Here it is anyway: "
  print e
