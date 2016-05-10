module Data.TyError where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Location
import           Data.Token
import           Data.Type
import           System.Console.ANSI

-- | Possible Type Errors that the type checker may emit
data TyError = UnboundVarE Id
            -- ^ A free variable was found for which no type exists
            -- (i.e. undefined variable).

             | DeferE Id
            -- ^ When typechecking this expression a free variable was found
            -- whose type was broken (an error was found whilst checking it).

             | UnificationE (Ty Id) (Ty Id) (Maybe (Ty Id, Ty Id))
            -- ^ Could not unify two types. Optionally, the error occurred in
            -- the unification of larger types which may also be given.

             | OccursE (Ty Id)
            -- ^ When unifying a variable with a type, we found the variable
            -- within the type (A cycle was detected in the occurs check). The
            -- type containing the cycle is given with cyclic components
            -- replaced with a special '*' variable.

             | CtxE String (Located TyError)
            -- ^ Adds context to an error in the form of a label and a location.
               deriving (Eq, Show)

-- | Check whether the root error is a deferral.
isDeferral :: TyError -> Bool
isDeferral (DeferE _)  = True
isDeferral (CtxE _ le) = isDeferral (dislocate le)
isDeferral _           = False

printError :: FilePath -> BS.ByteString -> TyError -> IO ()
printError fname input (CtxE root chain@(L rootSp _)) = do
  newLine
  setSGR [ SetUnderlining SingleUnderline
         , SetColor Foreground Vivid Red
         , SetConsoleIntensity BoldIntensity
         ]
  errHead rootSp; putStr "Error in the "; putStrLn root
  setSGR []; newLine
  unwind True chain
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

    indentS n msg = putStr (replicate (4*n) ' ') >> putStrLn msg
    indentBS  msg = putStr "    " >> BS.putStrLn msg

    unwind _ (L _ (UnboundVarE x))          = indentS 1 $ "Unbound variable '" ++ x ++ "'."
    unwind _ (L _ (DeferE x))               = do
      indentS 1 $ "Deferred because of an error found whilst checking '" ++ x ++ "'."
    unwind _ (L _ (OccursE t))              = do
      indentS 1 "Attempted to construct an infinite type!"
      indentS 2 $ show t
    unwind _ (L _ (UnificationE te ta ctx)) = do
      indentS 1 "Failed to unify types:"
      indentS 2 $ "Expected: " ++ show te
      indentS 2 $ "Actual:   " ++ show ta
      case ctx of
        Nothing -> return ()
        Just (pte, pta) -> do
          newLine
          indentS 1 "Whilst trying to unify:"
          indentS 2 $ "          " ++ show pte
          indentS 2 $ "    with: " ++ show pta

    unwind _ (L sp (CtxE lbl (L sp' (CtxE lbl' le@(L sp'' _)))))
      | sp == sp' = do
      unwind False le
      newLine; setSGR [SetColor Foreground Dull Red]
      errHead sp''; mapM_ putStr ["In the ", lbl', " of the "]; putStrLn lbl
      setSGR []; newLine
      nest sp' sp''

    unwind False (L sp (CtxE _ le@(L sp' _))) | sp == sp' = unwind False le

    unwind _ (L _  (CtxE lbl le@(L sp _))) = do
      unwind False le
      newLine; setSGR [SetColor Foreground Dull Red]
      errHead sp; putStr "In the "; putStrLn lbl
      setSGR []; newLine
      snip sp

printError _ _ e = do
  putStrLn "*** INTERNAL ERROR ***"
  putStrLn "An error was returned without a root location"
  putStrLn "Here it is anyway: "
  print e
  putStrLn ""
