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
  | AndThen
  | Anon
  | Gen
  | Range
  -- Punctuation
  | Comma
  | Semi
  | VBar
  -- Literals
  | Atom Id
  | Num Double
  | Str String
  deriving Eq

data Lexeme = L Token Int Int deriving Eq

instance Show Token where
  show Bra       = "["
  show Ket       = "]"
  show LPar      = "("
  show RPar      = ")"
  show Eof       = "\0"
  show (Ident x) = x
  show (BinOp x) = x
  show (MonOp x) = x
  show Define    = "define"
  show Else      = "else"
  show Function  = "function"
  show If        = "if"
  show In        = "in"
  show Let       = "let"
  show Then      = "then"
  show When      = "when"
  show AndThen   = ">>"
  show Anon      = "_"
  show Gen       = "<-"
  show Range     = ".."
  show Comma     = ","
  show Semi      = ";"
  show VBar      = "|"
  show (Atom x)  = "#" ++ x
  show (Num n)   = show n
  show (Str s)   = show s

instance Show Lexeme where
  show (L t l c) = concat [show t, " at line ", show l, ", column ", show c]

keywords :: H.Map String Token
keywords = H.fromList (keywords ++ binOps ++ monOps)
  where
    unwrap kw    = (show kw, kw)
    wrap ctr str = (str, ctr str)
    keywords = map unwrap
               [ Define, Else, Function, If
               , In, Let, Then, When
               , Anon, AndThen
               , Gen, Range
               ]
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
