module Token where

import qualified Data.HashMap as H

type Id = String
data Token =
  -- Brackets
    Bra | Ket | LPar | RPar
  -- Control
  | Eof
  -- Identifiers
  | Ident Id
  | BinOp Id
  | MonOp Id
  -- Keywords
  | Define | Else | Function | If | In | Let | Then | When
  -- Punctuation
  | AndThen {- '>>' -}
  | Anon    {- '_'  -}
  | Gen     {- '<-' -}
  | Range   {- '..' -}
  -- Punctuation
  | Comma   {- ','  -}
  | Semi    {- ';'  -}
  | VBar    {- '|'  -}
  -- Literals
  | Atom Id
  | Num Double
  | Str String
  deriving (Eq, Show)

data Lexeme = L Token Int Int deriving (Eq, Show)

keywords :: H.Map String Token
keywords = H.fromList (keywords ++ binOps ++ monOps)
  where
    keywords = [ ("define",   Define)
               , ("else",     Else)
               , ("function", Function)
               , ("if",       If)
               , ("in",       In)
               , ("let",      Let)
               , ("then",     Then)
               , ("when",     When)

               , ("_",        Anon)
               , (">>",       Then)

               , ("<-",       Gen)
               , ("..",       Range)
               ]
    wrap ctr str = (str, ctr str)
    binOps = map (wrap BinOp)
             [ "+", "-", "*", "/"
             , "and", "or"
             , "=", ">=", ">", "<=", "<", "<>"
             , ":", "++", "^"
             ]
    monOps = map (wrap MonOp)
             [ "~"
             , "not"
             ]

lookupKw :: String -> Maybe Token
lookupKw = flip H.lookup keywords
