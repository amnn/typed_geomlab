{
module Lexer (scanTokens) where
}

%wrapper "posn"

$ws     = [\t\n\r\ ]
$digit  = 0-9
$alpha  = [a-zA-Z]
$opchr  = [\.\!\#\$\%\&\*\+\-\/\:\<\=\>\?\@\^\~]
$break  = [\n\r\0]

@ident = [$alpha _][$alpha $digit _]*
@op    = $opchr+
@num   = $digit+(\.$digit+)?(E[\+\-]?$digit+)?
@str   = \"[^$break\"]*\"
@comment = \{.*\}

geomlab :-
  $ws+ ;
  @comment ;

  "#" @ident {\_ (_:id) -> Atom id }
  "#" @op    {\_ (_:id) -> Atom id }

  "[" {\_ _ -> Bra  }
  "]" {\_ _ -> Ket  }
  "(" {\_ _ -> LPar }
  ")" {\_ _ -> RPar }

  "," {\_ _ -> Comma }
  ";" {\_ _ -> Semi }
  "|" {\_ _ -> VBar }

  @ident {\_ id -> Ident id }
  @op    {\_ id -> Op id }
  @str   {\_ _  -> Str }
  @num   {\_ _  -> Num }
{
type Id = String
data Token =
  -- Brackets
    Bra | Ket | LPar | RPar
  -- Identifiers
  | Ident Id | Op Id
  -- Literals
  | Atom Id | Num | Str
  -- Punctuation
  | Comma | Semi | VBar
  deriving (Eq, Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
