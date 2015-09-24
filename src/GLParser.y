{
module GLParser where

import Token
import Shape
import Expr
import Patt

import Lexer
}

%name        parseExpr Top
%error     { parseError }
%lexer     { getToken } { L Eof l c }
%monad     { Alex }
%tokentype { Lexeme }

%token '('      { L LPar l c }
       ')'      { L RPar l c }
       '['      { L Bra l c }
       ']'      { L Ket l c }
       ','      { L Comma l c }
       ';'      { L Semi l c }
       '|'      { L VBar l c }
       num      { L (Num   $$) l c }
       str      { L (Str   $$) l c }
       atom     { L (Atom  $$) l c }
       ident    { L (Ident $$) l c }
       op       { L (Op    $$) l c }
       define   { L Define l c }
       function { L Function l c }
       else     { L Else l c }
       if       { L If l c }
       in       { L In l c }
       let      { L Let l c }
       then     { L Then l c }
       when     { L When l c }
       '_'      { L Anon l c }
       '>>'     { L AndThen l c }
       '+'      { L Plus l c }
       '-'      { L Minus l c }
       '*'      { L Mul l c }
       '/'      { L Div l c }
       and      { L And l c }
       '~'      { L Neg l c }
       not      { L Not l c }
       or       { L Or l c }
       '='      { L Eq l c }
       '>='     { L GEq l c }
       '>'      { L Gt l c }
       '<='     { L LEq l c }
       '<'      { L Lt l c }
       '<>'     { L NEq l c }
       ':'      { L Cons l c }
       '<-'     { L Gen l c }
       '++'     { L ListCat l c }
       '..'     { L Range l c }
       '^'      { L StrCat l c }

%%
-- Top Level
Top ::      { [Para] }
Top : Paras { reverse $1 }

Paras ::            { [Para] }
Paras : Para        { [$1] }
      | Paras Para  { $2 : $1 }

Para ::                { Para }
Para : define Decl ';' { declToDef $2 }
     | Expr ';'        { Eval $1 }

Decl ::               { Decl }
Decl : ident '=' Expr { Decl $1 $3 }
     | FnBody         {% fnToDecl (reverse $1) }

FnBody ::                 { [FnArm] }
FnBody : FnArm            { [$1] }
       | FnBody '|' FnArm { $3 : $1 }

FnArm ::                                 { FnArm }
FnArm : ident Formals '=' Expr           { FnArm $1 $2 $4 Nothing }
      | ident Formals '=' Expr when Expr { FnArm $1 $2 $4 (Just $6) }

-- Expressions
Expr ::                      { Expr }
Expr : let Decl in Expr      { declToLet $2 $4 }
     | function Formals Expr { FnE [FnArm "" $2 $3 Nothing] }
     | Cond                  { $1 }
     | Expr '>>' Cond        { SeqE $1 $3 }

Cond ::                            { Expr }
Cond : if Expr then Expr else Expr { IfE $2 $4 $6 }
     | Term                        { $1 }

Term ::       { Expr }
Term : Factor { $1 }
     | {- TODO -} { nilS }

Factor ::        { Expr }
Factor : Primary { $1 }
       | {- TODO -} { nilS }

Primary ::                      { Expr }
Primary : num                   { numS $1 }
        | atom                  { atomS $1 }
        | str                   { strS $1 }
        | ident                 { VarE $1 }
        | ident '(' Actuals ')' { AppE (VarE $1) (reverse $3) }
        | '[' ListExpr ']'      { $2 }
        | '(' Expr ')'          { $2 }

Actuals ::                 { [Expr] }
Actuals : {- empty -}      { [] }
        | Expr             { [$1] }
        | Actuals ',' Expr { $3 : $1 }

ListExpr ::               { Expr }
ListExpr : Actuals        { enlist $1 }
         | Expr '..' Expr { RangeE $1 $3 }
         | Expr '|' Gens  { ListCompE $1 (reverse $3) }

Gens ::             { [GenLvl] }
Gens : Gen          { [$1] }
     | Gens ',' Gen { $3 : $1 }

Gen ::                         { GenLvl }
Gen : Patt '<-' Expr           { GenLvl $1 $3 Nothing }
    | Patt '<-' Expr when Expr { GenLvl $1 $3 (Just $5) }

-- Pattern DSL
Formals ::              { [Patt] }
Formals : '(' Patts ')' { reverse $2 }

Patts ::               { [Patt] }
Patts : {- empty -}    { [] }
      | Patt           { [$1] }
      | Patts ',' Patt { $3 : $1 }

Patt ::                  { Patt }
Patt : PattFactor        { $1 }
     | Patt '+' NumChain { OffsetP $1 $3 }

NumChain ::                 { Double }
NumChain : num              { $1 }
         | NumChain '+' num { $1 + $3 }

PattFactor ::                          { Patt }
PattFactor : PattPrim                  { $1 }
           | PattPrimCons ':' PattPrim { enlist1 ($3 : $1) }

PattPrimCons ::                          { [Patt] }
PattPrimCons : PattPrim                  { [$1] }
             | PattPrimCons ':' PattPrim { $3 : $1 }

PattPrim ::                 { Patt }
PattPrim : ident            { VarP $1 }
         | atom             { atomS $1 }
         | '_'              { AnonP }
         | num              { numS $1 }
         | str              { strS $1 }
         | '(' Patt ')'     { $2 }
         | '[' ListPatt ']' { enlist $2 }

ListPatt ::                  { [Patt] }
ListPatt : {- empty -}       { [] }
         | Patt              { [$1] }
         | ListPatt ',' Patt { $3 : $1 }

{
-- TODO: Errors should contain positions (line + column)
fnToDecl :: [FnArm] -> Alex Decl
fnToDecl body@(a:as)
  | any (name a /=) (map name as)   = alexError "Parse Error, all function names need to match."
  | any (arity a /=) (map arity as) = alexError "Parse Error, all arities must match."
  | otherwise                       = return (Decl (name a) (FnE body))
  where
    name  (FnArm id _ _ _) = id
    arity (FnArm _ fs _ _) = length fs

fnToDecl []     = alexError "Parse Error, Empty function definition."

parseError :: Lexeme -> Alex a
parseError (L tok l c) = alexError msg
  where
    msg = concat ["Parse Error, near ", show tok
                 , " at line ", show l, ", column ", show c, "."]

parse :: String -> [Para]
parse input = either error id (runAlex input parseExpr)
}
