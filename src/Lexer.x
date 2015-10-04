{
module Lexer where

import Data.Char (chr)

import Token
}

%wrapper "monadUserState"

$ws     = [\t\n\r\ ]
$digit  = 0-9
$alpha  = [a-zA-Z]
$opchr  = [\.\!\#\$\%\&\*\+\-\/\:\<\=\>\?\@\^\~]
$break  = [\n\r\0]

@ident = [$alpha _][$alpha $digit _]*
@op    = $opchr+
@dec   = $digit+(\.$digit+)?
@exp   = E[\+\-]?$digit+
@num   = $digit+(\.$digit+)?(@exp)?

@strbegin = \"[^$break\"]*

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

  @ident { ident }
  @op    { op    }

  @num        { strTok (Num . read) }
  @strbegin\" { strTok (Str . dequote) }

  -- Error cases
  "}"                     { scanError "#bracematch" }
  @strbegin               { scanError "#string" }
  "#"[^$alpha$opchr _]    { scanError "#idop" }
  @dec"E"[\+\-]?[^$digit] { badToken }
  .                       { badToken }
{

data AlexUserState = AlexUserState { syms :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { syms = 0 }

incSym :: Alex Int
incSym = Alex $ \s@AlexState{alex_ust=ust} ->
           let x = syms ust in
               Right (s{alex_ust = ust{syms = x + 1}}, x)

genSym :: Alex Id
genSym = ("var" ++) . show <$> incSym

dequote :: String -> String
dequote = tail . init

strTok :: (String -> Token) -> AlexAction Lexeme
strTok t (pos, _, _, rest) len = return (mkLex token pos)
  where
    token = t (take len rest)

tok :: Token -> AlexAction Lexeme
tok t = strTok (const t)

ident :: AlexAction Lexeme
ident = strTok $ \id ->
          case lookupKw id of
            Just t -> t
            Nothing -> Ident id

op :: AlexAction Lexeme
op = strTok $ \id ->
       case lookupKw id of
         Just t  -> t
         Nothing -> BinOp id

mkLex :: Token -> AlexPosn -> Lexeme
mkLex tok (AlexPn _ line col) = L tok line col

skipComment :: AlexAction Lexeme
skipComment _ _ = alexGetInput >>= go 1
  where
    go 0   input = alexSetInput input >> alexMonadScan
    go lvl input = case alexGetByte input of
                     Nothing -> scanError "#comment" input 0
                     Just (b, rest)
                       | fromByte b == '}' -> go (lvl-1) rest
                       | fromByte b == '{' -> go (lvl+1) rest
                       | otherwise         -> go  lvl    rest
    fromByte     = chr . fromIntegral

alexEOF = return (L Eof 0 0)

scanError :: String -> AlexAction a
scanError msg (pos, _, _, str) _ =
  alexError (concat ["Syntax Error, ", msg, fmtPosn pos])
  where
    fmtPosn (AlexPn _ line col) =
      concat [" at line ", show line, ", column ", show col, "."]

badToken :: AlexAction a
badToken = scanError "#badtoken"

getToken :: (Lexeme -> Alex a) -> Alex a
getToken = (alexMonadScan >>=)
}
