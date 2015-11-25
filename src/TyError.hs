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
printError fname input (CtxE root (L topSp eChain)) = do
  newLine
  setSGR [ SetUnderlining SingleUnderline
         , SetColor Foreground Vivid Red
         , SetConsoleIntensity BoldIntensity
         ]
  errHead topSp; putStr "Error in "; putStrLn root
  setSGR []; newLine
  unwind topSp eChain
  newLine

  where
    errHead sp | (P l c) <- start sp =
      mapM_ putStr [fname, ":", show l, ":", show c, ": "]

    snip sp = mapM_ (indentBS 1) . BS.lines
            . BS.take (fromIntegral (width sp))
            . BS.drop (fromIntegral (offset sp))
            $ input

    newLine = putStrLn ""

    indentS  n msg = do { putStr $ replicate (4*n) ' '; putStrLn msg }
    indentBS n msg = do { putStr $ replicate (4*n) ' '; BS.putStrLn msg }

    unwind _ (UnboundVarE x) = indentS 1 $ "unbound variable '" ++ x ++ "'"
    unwind _ UnificationE    = indentS 1 "unification error"
    unwind _ OccursE         = indentS 1 "cyclicity error"

    unwind sp (CtxE lbl (L sp' (CtxE lbl' (L sp'' e))))
      | sp == sp' = do
      unwind sp'' e; newLine
      setSGR [SetColor Foreground Dull Red]
      errHead sp'; mapM_ putStr ["In the ", lbl', " of the "]; putStrLn lbl
      setSGR []
      newLine; snip sp'

    unwind _ (CtxE lbl (L sp e)) = do
      unwind sp e
      newLine; errHead sp
      putStr "In the "; putStrLn lbl
      newLine; snip sp

printError _ _ e = do
  putStrLn "*** INTERNAL ERROR ***"
  putStrLn "An error was returned without a root location"
  putStrLn "Here it is anyway: "
  print e

{-
-- Check for Duplicate Spans.
-- Print the top most location
-- Print the rest in reverse order
  -- Title with file and line/column indicator and label
  -- Snippet of code

If two contexts have the same span, the outer is a child context, and the
inner is a parent context, and we print them together.

[decl -> list] -> [element -> function] -> argument -> let

print decl

[decl* -> list] -> [element -> function] -> argument -> let

print element of list

[element* -> function] -> argument -> let

print argument of function

*argument -> let

print let

element of list; argument of function application

[element -> function] -> argument
argument of function application

discard grandparent, print parent compacted with child
recurse with child still attached.


not.geom:1:2: Error in definition of 'not'

    unbound variable 'x'

not.geom:3:4: In the 1st argument of the function application

    foo(*x*, y)

not.geom:5:6: In the element of the list

    [*foo(x, y)*]

-}
