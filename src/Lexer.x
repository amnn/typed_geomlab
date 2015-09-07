{
module Lexer (scanTokens) where
import Data.Char (chr)
}

%wrapper "monad"

$ws     = [\t\n\r\ ]
$digit  = 0-9
$alpha  = [a-zA-Z]
$opchr  = [\.\!\#\$\%\&\*\+\-\/\:\<\=\>\?\@\^\~]
$break  = [\n\r\0]

@ident = [$alpha _][$alpha $digit _]*
@op    = $opchr+
@num   = $digit+(\.$digit+)?(E[\+\-]?$digit+)?
@str   = \"[^$break\"]*\"

geomlab :-
  $ws+     { skip }
  "{"      { skipComment }

  "#" @ident { strTok (Atom . tail) }
  "#" @op    { strTok (Atom . tail) }

  "[" { tok Bra  }
  "]" { tok Ket  }
  "(" { tok LPar }
  ")" { tok RPar }

  "," { tok Comma }
  ";" { tok Semi  }
  "|" { tok VBar  }

  @ident { strTok Ident }
  @op    { strTok Op    }

  @str   { tok Str }
  @num   { tok Num }

{
type Id = String
data Token =
  -- Brackets
    Bra | Ket | LPar | RPar
  -- Control
  | Eof
  -- Identifiers
  | Ident Id | Op Id
  -- Literals
  | Atom Id | Num | Str
  -- Punctuation
  | Comma | Semi | VBar
  deriving (Eq, Show)

data Lexeme = L Token Int Int deriving (Eq, Show)

strTok :: (String -> Token) -> AlexAction Lexeme
strTok t (pos, _, _, rest) len = return (mkLex token pos)
  where
    token = t (take len rest)

tok :: Token -> AlexAction Lexeme
tok t = strTok (const t)

mkLex :: Token -> AlexPosn -> Lexeme
mkLex tok (AlexPn _ line col) = L tok line col

skipComment :: AlexAction Lexeme
skipComment _ _ = alexGetInput >>= go 1
  where
    go 0   input = alexSetInput input >> alexMonadScan
    go lvl input = case alexGetByte input of
                     Nothing -> alexError "#comment"
                     Just (b, rest)
                       | fromByte b == '}' -> go (lvl-1) rest
                       | fromByte b == '{' -> go (lvl+1) rest
                       | otherwise         -> go  lvl    rest
    fromByte     = chr . fromIntegral

alexEOF = return (L Eof 0 0)

scanTokens :: String -> [Lexeme]
scanTokens input = either error id res
  where
    res = runAlex input loop
    loop = do l@(L tok _ _) <- alexMonadScan
              if tok == Eof
              then return []
              else do rest <- loop
                      return (l:rest)
}
