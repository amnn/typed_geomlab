{
module GLParser where
import Lexer
}

%name        parseExpr Top
%error     { parseError }
%lexer     { getToken } { L Eof l c }
%monad     { Alex }
%tokentype { Lexeme }

%token num { L Num   l c }
       str { L Str   l c }
       '(' { L LPar  l c }
       ')' { L RPar  l c }
       '[' { L Bra   l c }
       ']' { L Ket   l c }
       ',' { L Comma l c }
       ';' { L Semi  l c }
       '|' { L VBar  l c }
       atom   { L (Atom  $$) l c }
       ident  { L (Ident $$) l c }
       op     { L (Op    $$) l c }
       define { L Define l c }
       else   { L Else l c }
       if     { L If l c }
       in     { L In l c }
       let    { L Let l c }
       then   { L Then l c }
       when   { L When l c }
       '_'    { L Anon l c }
       '>>'   { L AndThen l c }
       '+'    { L Plus l c }
       '-'    { L Minus l c }
       '*'    { L Mul l c }
       '/'    { L Div l c }
       and    { L And l c }
       '~'    { L Neg l c }
       not    { L Not l c }
       or     { L Or l c }
       '='    { L Eq l c }
       '>='   { L GEq l c }
       '>'    { L Gt l c }
       '<='   { L LEq l c }
       '<'    { L Lt l c }
       '<>'   { L NEq l c }
       ':'    { L Cons l c }
       '<-'   { L Gen l c }
       '++'   { L ListCat l c }
       '..'   { L Range l c }
       '^'    { L StrCat l c }

%%

Top : Paras { Top (reverse $1) }

Paras : Para { [$1] }
      | Paras Para  { $2 : $1 }

Para : define ident '=' Expr ';' { Def $2 $4 }
     | Expr ';'                  { $1 }

Expr : num { NumLit }

{
data Expr = NumLit
          | BoolLit
          | StrLit
          | NilLit
          | AtomLit Id
          | VarExpr Id
          | ConsExpr Expr Expr
          | IfExpr Expr Expr Expr
          | Fn [Id] Expr
          | LetExpr Id Expr Expr
          | Def Id Expr
          | Top [Expr]
            deriving (Eq, Show)

parseError :: Lexeme -> Alex a
parseError (L tok l c) = alexError msg
  where
    msg = concat ["Parse Error, near ", show tok
                 , " at line ", show l, ", column ", show c, "."]

parse :: String -> Expr
parse input = either error id (runAlex input parseExpr)
}
