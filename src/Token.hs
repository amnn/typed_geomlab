module Token where

import qualified Data.HashMap as H

type Id = String
data Token =
  -- Brackets
    Bra | Ket | LPar | RPar
  -- Control
  | Eof
  -- Identifiers
  | Ident Id | Op Id
  -- Keywords
  | Define | Else | Function | If | In | Let | Then | When
  -- Literals
  | Atom Id
  | Num Double
  | Str String
  -- Primitive Operators
  | Anon    {- '_'  -}
  | AndThen {- '>>' -}
  -- Numeric Operators
  | Plus    {- '+' -}
  | Minus   {- '-' -}
  | Mul     {- '*' -}
  | Div     {- '/' -}
  -- Boolean Operators
  | And     {- 'and' -}
  | Neg     {- '~'   -}
  | Not     {- 'not' -}
  | Or      {- 'or'  -}
  -- Relational Operations
  | Eq      {- '='  -}
  | GEq     {- '>=' -}
  | Gt      {- '>'  -}
  | LEq     {- '<=' -}
  | Lt      {- '<'  -}
  | NEq     {- '<>' -}
  -- List Operators
  | Cons    {- ':'  -}
  | Gen     {- '<-' -}
  | ListCat {- '++' -}
  | Range   {- '..' -}
  | StrCat  {- '^'  -}
  -- Punctuation
  | Comma | Semi | VBar
  deriving (Eq, Show)

data Lexeme = L Token Int Int deriving (Eq, Show)

keywords :: H.Map String Token
keywords = H.fromList [ ("define",   Define)
                      , ("else",     Else)
                      , ("function", Function)
                      , ("if",       If)
                      , ("in",       In)
                      , ("let",      Let)
                      , ("then",     Then)
                      , ("when",     When)

                      , ("_",        Anon)
                      , (">>",       Then)

                      , ("+",        Plus)
                      , ("-",        Minus)
                      , ("*",        Mul)
                      , ("/",        Div)

                      , ("and",      And)
                      , ("~",        Neg)
                      , ("not",      Not)
                      , ("or",       Or)

                      , ("=",        Eq)
                      , (">=",       GEq)
                      , (">",        Gt)
                      , ("<=",       LEq)
                      , ("<",        Lt)
                      , ("<>",       NEq)

                      , (":",        Cons)
                      , ("<-",       Gen)
                      , ("++",       ListCat)
                      , ("..",       Range)
                      , ("^",        StrCat)
                      ]

lookupKw :: String -> Maybe Token
lookupKw = flip H.lookup keywords
