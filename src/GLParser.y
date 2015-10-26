{
module GLParser where

import Lexer
import Literal
import Patt
import Sugar
import Token

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
       '_'      { L Anon l c }
       '>>'     { L AndThen l c }
       '<-'     { L Gen l c }
       '..'     { L Range l c }
       '+'      { L (BinOp "+") l c }
       '-'      { L (BinOp "-") l c }
       '='      { L (BinOp "=") l c }
       ':'      { L (BinOp ":") l c }
       num      { L (Num   $$) l c }
       str      { L (Str   $$) l c }
       atom     { L (Atom  $$) l c }
       ident    { L (Ident $$) l c }
       binop    { L (BinOp $$) l c }
       monop    { L (MonOp $$) l c }
       define   { L Define l c }
       function { L Function l c }
       else     { L Else l c }
       if       { L If l c }
       in       { L In l c }
       let      { L Let l c }
       then     { L Then l c }
       when     { L When l c }

%%
-- Top Level
Top ::      { [Para Sugar] }
Top : Paras { reverse $1 }

Paras ::            { [Para Sugar] }
Paras : Para        { [$1] }
      | Paras Para  { $2 : $1 }

Para ::                { Para Sugar }
Para : define Decl ';' { declToDef $2 }
     | Expr ';'        { Eval $1 }

Decl ::               { Decl }
Decl : Id '=' Expr { Decl $1 $3 }
     | FnBody         {% fnToDecl (reverse $1) }

FnBody ::                 { [FnArm] }
FnBody : FnArm            { [$1] }
       | FnBody '|' FnArm { $3 : $1 }

FnArm ::                          { FnArm }
FnArm : FnLhs '=' Expr           { mkFnArm $1 $3 Nothing }
      | FnLhs '=' Expr when Expr { mkFnArm $1 $3 (Just $5) }

FnLhs :: { (Id, [Patt]) }
FnLhs : monop '(' Patt ')'          { ($1, [$3]) }
      | binop '(' Patt ',' Patt ')' { ($1, [$3, $5]) }
      | ident Formals               { ($1, $2) }

Id :: { Id }
Id : ident { $1 }
   | monop { $1 }
   | binop { $1 }

-- Expressions
Expr ::                      { Sugar }
Expr : Cond                  { $1 }
     | let Decl in Expr      { declToLet $2 $4 }
     | function Formals Expr { FnS [FnArm "" $2 $3 Nothing] }
     | Cond '>>' Expr        { SeqS $1 $3 }

ExprOrSect ::                      { Sugar }
ExprOrSect : let Decl in Expr      { declToLet $2 $4 }
           | function Formals Expr { FnS [FnArm "" $2 $3 Nothing] }
           | CondOrSect            { $1 }
           | Cond '>>' Expr        { SeqS $1 $3 }

Cond ::                            { Sugar }
Cond : Term                        { $1 }
     | if Cond then Cond else Cond { IfS $2 $4 $6 }

CondOrSect ::                            { Sugar }
CondOrSect : TermOrSect                  { $1 }
           | if Cond then Cond else Cond { IfS $2 $4 $6 }

Term ::       { Sugar }
Term : Factor { $1 }
     | OpTree { mkOpExpr $1 }

TermOrSect ::             { Sugar }
TermOrSect : Factor       { $1 }
           | Factor BinOp { RSectS $1 $2 }
           | OpTree       { mkOpExpr $1 }
           | OpTree BinOp { RSectS (mkOpExpr $1) $2 }

OpTree ::                    { OpTree Sugar }
OpTree : Factor BinOp Factor { Op $2 (Leaf $1) (Leaf $3) }
       | OpTree BinOp Factor { fixPrec $ Op $2 $1 (Leaf $3) }

BinOp ::             { Id }
BinOp : BinOpNoMinus { $1 }
      | '-'          { "-" }

BinOpNoMinus ::      { Id }
BinOpNoMinus : '+'   { "+" }
             | ':'   { ":" }
             | '='   { "=" }
             | binop { $1 }

Factor ::             { Sugar }
Factor : Primary      { $1 }
       | monop Factor { AppS $1 [$2] }
       | '-' Factor   { AppS "~" [$2] }

Primary ::                          { Sugar }
Primary : num                       { numB $1 }
        | atom                      { atomB $1 }
        | str                       { strB $1 }
        | ident                     { VarS $1 }
        | ident '(' Actuals ')'     { AppS $1 (reverse $3) }
        | '[' ListExpr ']'          { $2 }
        | '(' monop ')'             { VarS $2 }
        | '(' BinOp ')'             { VarS $2 }
        | '(' BinOpNoMinus Term ')' { LSectS $2 $3 }
        | '(' ExprOrSect ')'        { $2 }

Actuals ::                 { [Sugar] }
Actuals : {- empty -}      { [] }
        | Expr             { [$1] }
        | Actuals ',' Expr { $3 : $1 }

ListExpr ::               { Sugar }
ListExpr : Actuals        { enlist $1 }
         | Expr '..' Expr { RangeS $1 $3 }
         | Expr '|' Gens  { ListCompS $1 (reverse $3) }

Gens ::                        { [Gen] }
Gens : Patt '<-' Expr          { [GenB $1 $3] }
     | Gens ',' Patt '<-' Expr { GenB $3 $5 : $1 }
     | Gens when Expr          { FilterB $3 : $1 }

-- Pattern DSL
Formals ::              { [Patt] }
Formals : '(' Patts ')' { reverse $2 }

Patts ::               { [Patt] }
Patts : {- empty -}    { [] }
      | Patt           { [$1] }
      | Patts ',' Patt { $3 : $1 }

Patt ::             { Patt }
Patt : PattFactor   { $1 }
     | Patt '+' num { OffsetP $1 $3 }

PattFactor ::                          { Patt }
PattFactor : PattPrim                  { $1 }
           | PattPrimCons ':' PattPrim { enlist1 ($3 : $1) }

PattPrimCons ::                          { [Patt] }
PattPrimCons : PattPrim                  { [$1] }
             | PattPrimCons ':' PattPrim { $3 : $1 }

PattPrim ::                 { Patt }
PattPrim : ident            { VarP $1 }
         | atom             { atomB $1 }
         | '_'              {% fmap VarP genSym }
         | num              { numB $1 }
         | str              { strB $1 }
         | ident Formals    { CtrP $1 $2 }
         | '(' Patt ')'     { $2 }
         | '[' ListPatt ']' { enlist $2 }

ListPatt ::                  { [Patt] }
ListPatt : {- empty -}       { [] }
         | Patt              { [$1] }
         | ListPatt ',' Patt { $3 : $1 }

{
-- TODO: Errors should contain positions (line + column)

-- Function Declarations
mkFnArm = uncurry FnArm

fnToDecl :: [FnArm] -> Alex Decl
fnToDecl body@(a:as)
  | any (name a /=) (map name as)   = alexError "Parse Error, all function names need to match."
  | any (arity a /=) (map arity as) = alexError "Parse Error, all arities must match."
  | otherwise                       = return (Decl (name a) (FnS body))
  where
    name  (FnArm id _ _ _) = id
    arity (FnArm _ fs _ _) = length fs

fnToDecl [] = error "Internal Error, Empty function definition."

-- Operator Associativity Parsing
mkOpExpr :: OpTree Sugar -> Sugar
mkOpExpr (Leaf e)   = e
mkOpExpr (Op i l r) = AppS i [mkOpExpr l, mkOpExpr r]

parseError :: Lexeme -> Alex a
parseError l = alexError msg
  where
    msg = concat ["Parse Error, near ", show l]
}
