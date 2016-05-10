{
{-# OPTIONS_GHC -w #-}

module Lexer ( Alex
             , alexError
             , alexMonadScan
             , getToken
             , runAlex
             ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (chr)
import           Data.Location
import           Data.Token
}

%wrapper "monad-bytestring"

$ws     = [\t\n\r\ ]
$digit  = 0-9
$alpha  = [a-zA-Z]
$opchr  = [\.\!\#\$\%\&\*\+\-\/\:\<\=\>\?\@\^\~]
$break  = [\n\r\0]

@ident = [$alpha _][$alpha $digit _]*
@op    = $opchr+
@dec   = $digit+(\.$digit+)?
@exp   = E[\+\-]?$digit+
@num   = \-?@dec(@exp)?

@strbegin = \"[^$break\"]*

geomlab :-
  $ws+     { skip }
  "{"      { skipComment }

  "#" @ident { strTok (Atom . BS.unpack . BS.tail) }
  "#" @op    { strTok (Atom . BS.unpack . BS.tail) }

  "[" { tok Bra  }
  "]" { tok Ket  }
  "(" { tok LPar }
  ")" { tok RPar }

  "," { tok Comma }
  ";" { tok Semi  }
  "|" { tok VBar  }

  @num        { strTok (Num . read . BS.unpack) }
  @strbegin\" { strTok (Str . dequote) }

  @ident { ident }
  @op    { op    }

  -- Error cases
  "}"                     { scanError "#bracematch" }
  @strbegin               { scanError "#string" }
  "#"[^$alpha$opchr _]    { scanError "#idop" }
  @dec"E"[\+\-]?[^$digit] { badToken }
  .                       { badToken }
{

dequote :: BS.ByteString -> String
dequote = BS.unpack . BS.tail . BS.init

strTok :: (BS.ByteString -> Token) -> AlexAction Lexeme
strTok t (pos, _, rest, _) len = return (mkLex (fromIntegral len) str pos)
  where
    str = t (BS.take len rest)

tok :: Token -> AlexAction Lexeme
tok t = strTok (const t)

ident :: AlexAction Lexeme
ident = strTok $ \x ->
          let str = BS.unpack x
          in case lookupKw str of
            Just t -> t
            Nothing -> Ident str

op :: AlexAction Lexeme
op = strTok $ \x ->
       let str = BS.unpack x
       in case lookupKw str of
         Just t  -> t
         Nothing -> BinOp str

mkLex :: Int -> Token -> AlexPosn -> Lexeme
mkLex len t (AlexPn o l c) = L (S (P l c) o len) t

-- | Eat up nested comments and return the following token.
skipComment :: AlexAction Lexeme
skipComment _ _ = alexGetInput >>= go 1
  where
    go :: Int -> AlexInput -> Alex Lexeme
    go 0   input = alexSetInput input >> alexMonadScan
    go lvl input = case alexGetByte input of
                     Nothing -> scanError "#comment" input 0
                     Just (b, rest)
                       | fromByte b == '}' -> go (lvl-1) rest
                       | fromByte b == '{' -> go (lvl+1) rest
                       | otherwise         -> go  lvl    rest
    fromByte     = chr . fromIntegral

alexEOF :: Alex Lexeme
alexEOF = return (L Floating Eof)

scanError :: String -> AlexAction a
scanError msg (pos, _, str, _) _ =
  alexError (concat ["Syntax Error, ", msg, fmtPosn pos])
  where
    fmtPosn (AlexPn _ line col) =
      concat ["near ", BS.unpack str, " at line ", show line, ", column ", show col, "."]

badToken :: AlexAction a
badToken = scanError "#badtoken"

getToken :: (Lexeme -> Alex a) -> Alex a
getToken = (alexMonadScan >>=)
}
