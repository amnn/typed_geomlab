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
  show (BinOp x) = "\"" ++ x
  show (MonOp x) = "'" ++ x
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

data Assoc = LeftA  { pri :: Int }
           | RightA { pri :: Int }
             deriving (Eq, Show)

data OpTree a = Leaf a
              | Op Id (OpTree a) (OpTree a)
                deriving (Eq, Show)

assoc :: Id -> Assoc
assoc op
  | op `elem` ["or"]                            = LeftA  1
  | op `elem` ["and"]                           = LeftA  2
  | op `elem` ["=", "<", "<=", "<>", ">=", ">"] = LeftA  3
  | op `elem` ["++"]                            = RightA 4
  | op `elem` ["+", "-"]                        = LeftA  5
  | op `elem` ["^"]                             = LeftA  5
  | op `elem` ["."]                             = LeftA  5
  | op `elem` ["&"]                             = LeftA  5
  | op `elem` ["$"]                             = LeftA  6
  | op `elem` ["*", "/"]                        = LeftA  6
  | op `elem` ["div", "mod"]                    = LeftA  6
  | op `elem` [":"]                             = RightA 7
  | otherwise                                   = RightA 0

fixPrec :: OpTree a -> OpTree a
fixPrec (Op i (Op j ll lr) r)
  | shouldRot (assoc i) (assoc j) = (Op j ll (Op i lr r))
  where
    shouldRot (LeftA p)  a = p >  (pri a)
    shouldRot (RightA p) a = p >= (pri a)

fixPrec x = x

kws :: H.Map String Token
kws = H.fromList (keywords ++ binOps ++ monOps)
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
             , "div", "mod"
             , "=", ">=", ">", "<=", "<", "<>"
             , ":", "++", "^"
             , "."
             , "$", "&"
             ]
    monOps = map (wrap MonOp)
             [ "~"
             , "not"
             ]

lookupKw :: String -> Maybe Token
lookupKw = flip H.lookup kws
