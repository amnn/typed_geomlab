{
module GLParser where

import Lexer
import Literal
import Location
import Patt
import Sugar
import Token

}

%name        parseExpr Top
%error     { parseError }
%lexer     { getToken } { L s Eof }
%monad     { Alex }
%tokentype { Lexeme }

%token '('      { L s LPar }
       ')'      { L s RPar }
       '['      { L s Bra }
       ']'      { L s Ket }
       ','      { L s Comma }
       ';'      { L s Semi }
       '|'      { L s VBar }
       '_'      { L s Anon }
       '>>'     { L s AndThen }
       '<-'     { L s Gen }
       '..'     { L s Range }
       '+'      { L s (BinOp "+") }
       '-'      { L s (BinOp "-") }
       '='      { L s (BinOp "=") }
       ':'      { L s (BinOp ":") }
       num      { L s (Num   n) }
       str      { L s (Str   t) }
       atom     { L s (Atom  a) }
       ident    { L s (Ident i) }
       binop    { L s (BinOp b) }
       monop    { L s (MonOp m) }
       define   { L s Define }
       function { L s Function }
       else     { L s Else }
       if       { L s If }
       in       { L s In }
       let      { L s Let }
       then     { L s Then }
       when     { L s When }

%%
-- Top Level
Top ::      { [Para Sugar] }
Top : Paras { reverse $1 }

Paras ::            { [Para Sugar] }
Paras : Para        { [$1] }
      | Paras Para  { $2 : $1 }

Para ::                { Para Sugar }
Para : define Decl ';' { declToDef $2 }
     | Expr ';'        { Eval . dislocate . reify $ $1 }

Decl ::            { Located Decl }
Decl : Id '=' Expr { Decl <@> $1 <*> reify $3 }
     | FnBody      {% fnToDecl $ (reverse $1) }

FnBody ::                 { [Located FnArm] }
FnBody : FnArm            { [$1] }
       | FnBody '|' FnArm { $3 : $1 }

FnArm ::                         { Located FnArm }
FnArm : FnLhs '=' Expr           { mkFnArm $1 (reify $3) (pure Nothing) }
      | FnLhs '=' Expr when Expr { mkFnArm $1 (reify $3) (Just <@> (reify $5)) }

FnLhs ::                            { (Located Id, Located [Patt]) }
FnLhs : monop '(' Patt ')'          { (ident $1, loc [$3]) }
      | binop '(' Patt ',' Patt ')' { (ident $1, loc [$3, $5]) }
      | ident Formals               { (ident $1, $2) }

Id :: { Located Id }
Id : ident { ident $1 }
   | monop { ident $1 }
   | binop { ident $1 }

-- Expressions
Expr ::                      { Located Sugar }
Expr : Cond                  { $1 }
     | let Decl in Expr      { declToLet $2 (reify $4) }
     | function Formals Expr { $1 *> (FnS <@> loc [FnArm "" <@> $2 <*> reify $3 <*> pure Nothing]) }
     | Cond '>>' Expr        { SeqS <@> $1 <*> $3 }

ExprOrSect ::                      { Located Sugar }
ExprOrSect : let Decl in Expr      { declToLet $2 (reify $4) }
           | function Formals Expr { $1 *> (FnS <@> loc [FnArm "" <@> $2 <*> reify $3 <*> pure Nothing]) }
           | CondOrSect            { $1 }
           | Cond '>>' Expr        { SeqS <@> reify $1 <*> reify $3 }

Cond ::                            { Located Sugar }
Cond : Term                        { $1 }
     | if Cond then Cond else Cond { IfS <@> reify $2 <*> reify $4 <*> reify $6 }

CondOrSect ::                            { Located Sugar }
CondOrSect : TermOrSect                  { $1 }
           | if Cond then Cond else Cond { IfS <@> reify $2 <*> reify $4 <*> reify $6 }

Term ::       { Located Sugar }
Term : Factor { $1 }
     | OpTree { mkOpExpr $1 }

TermOrSect ::             { Located Sugar }
TermOrSect : Factor       { $1 }
           | Factor BinOp { LSectS <@> reify $1 <*> $2 }
           | OpTree       { mkOpExpr $1 }
           | OpTree BinOp { LSectS <@> (mkOpExpr $1) <*> $2 }

OpTree ::                    { OpTree (Located Sugar) }
OpTree : Factor BinOp Factor { Op (dislocate $2) (Leaf $1) (Leaf $3) }
       | OpTree BinOp Factor { fixPrec $ Op (dislocate $2) $1 (Leaf $3) }

BinOp ::             { Located Id }
BinOp : BinOpNoMinus { $1 }
      | '-'          { ident $1 }

BinOpNoMinus ::      { Located Id }
BinOpNoMinus : '+'   { ident $1 }
             | ':'   { ident $1 }
             | '='   { ident $1 }
             | binop { ident $1 }

Factor ::             { Located Sugar }
Factor : Primary      { $1 }
       | monop Factor { apply (ident $1) [$2] }
       | '-' Factor   { apply ($1 *> pure "~") [$2] }

Primary ::                          { Located Sugar }
Primary : num                       { val $1 }
        | atom                      { val $1 }
        | str                       { val $1 }
        | ident                     { val $1 }
        | ident '(' Actuals ')'     { apply (ident $1) (reverse $3) <* $4 }
        | '[' ListExpr ']'          { $1 *> $2 <* $3 }
        | '(' monop ')'             { $1 *> val $2 <* $3 }
        | '(' BinOp ')'             { $1 *> (VarS <@> $2) <* $3 }
        | '(' BinOpNoMinus Term ')' { $1 *> RSectS <@> $2 <*> reify $3 <* $4 }
        | '(' ExprOrSect ')'        { $1 *> $2 <* $3 }

Actuals ::                 { [Located Sugar] }
Actuals : {- empty -}      { [] }
        | Expr             { [$1] }
        | Actuals ',' Expr { $3 : $1 }

ListExpr ::               { Located Sugar }
ListExpr : Actuals        { enlist <@> loc (reify <@> $1) }
         | Expr '..' Expr { RangeS <@> reify $1 <*> reify $3 }
         | Expr '|' Gens  { ListCompS <@> reify $1 <*> loc (reverse $3) }

Gens ::                        { [Located Gen] }
Gens : Patt '<-' Expr          { [GenB <@> $1 <*> reify $3] }
     | Gens ',' Patt '<-' Expr { (GenB <@> $3 <*> reify $5) : $1 }
     | Gens when Expr          { (FilterB <@> reify $3) : $1 }

-- Pattern DSL
Formals ::              { Located [Patt] }
Formals : '(' Patts ')' { $1 *> loc (reverse $2) <* $3 }

Patts ::               { [Located Patt] }
Patts : {- empty -}    { [] }
      | Patt           { [$1] }
      | Patts ',' Patt { $3 : $1 }

Patt ::                          { Located Patt }
Patt : PattPrim                  { $1 }
     | PattPrimCons ':' PattPrim { enlist1 <@> loc ($3 : $1) }

PattPrimCons ::                          { [Located Patt] }
PattPrimCons : PattPrim                  { [$1] }
             | PattPrimCons ':' PattPrim { $3 : $1 }

PattPrim ::                 { Located Patt }
PattPrim : ident            { pat $1 }
         | atom             { pat $1 }
         | num              { pat $1 }
         | str              { pat $1 }
         | '_'              { $1 *> pure (VarP "_") }
         | '(' Patt ')'     { $1 *> $2 <* $3 }
         | '[' ListPatt ']' { $1 *> (enlist <@> loc $2) <* $3 }

ListPatt ::                  { [Located Patt] }
ListPatt : {- empty -}       { [] }
         | Patt              { [$1] }
         | ListPatt ',' Patt { $3 : $1 }

{

-- | Redefining fmap to get around the fact that Happy treats `$>` specially.
(<@>) :: Functor f => (a -> b) -> f a -> f b
(<@>) = (<$>)

-- Source Mapping
reify :: Located Sugar -> Located Sugar
reify ls@(L s _) = L s (LocS ls)

apply :: Located Id -> [Located Sugar] -> Located Sugar
apply x xs = AppS <@> x <*> loc (reify <@> xs)

val :: Lexeme -> Located Sugar
val = fmap trn
  where
    trn (Num n)   = numB n
    trn (Str s)   = strB s
    trn (Atom a)  = atomB a
    trn (Ident v) = VarS v
    trn (BinOp b) = VarS b
    trn (MonOp m) = VarS m
    trn _         = error "val: Not a value"

pat :: Lexeme -> Located Patt
pat = fmap trn
  where
    trn (Num n)   = numB n
    trn (Str s)   = strB s
    trn (Atom a)  = atomB a
    trn (Ident v) = VarP v
    trn (BinOp b) = VarP b
    trn (MonOp m) = VarP m
    trn _         = error "pat: Not a base pattern"

ident :: Lexeme -> Located Id
ident = fmap trn
  where
    trn (Ident v) = v
    trn (BinOp b) = b
    trn (MonOp m) = m
    trn _         = error "ident: Not an identifier"

-- TODO: Errors should contain positions (line + column)

-- Function Declarations
mkFnArm :: (Located Id, Located [Patt])
        -- ^ Function Interface
        -> Located Sugar
        -- ^ Function Body
        -> Located (Maybe Sugar)
        -- ^ Optional guard condition
        -> Located FnArm

mkFnArm (li, lps) lb lg = FnArm <@> li <*> lps <*> lb <*> lg

fnToDecl :: [Located FnArm] -> Alex (Located Decl)
fnToDecl body@(a:as)
  | any (name a /=) (map name as)   = alexError "Parse Error, all function names need to match."
  | any (arity a /=) (map arity as) = alexError "Parse Error, all arities must match."
  | otherwise                       = return (Decl (name a) <@> (FnS <@> loc body))
  where
    name  lfa | (FnArm id _ _ _) <- dislocate lfa = id
    arity lfa | (FnArm _ fs _ _) <- dislocate lfa = length fs

fnToDecl [] = error "fnToDecl: Empty function definition."

-- Operator Associativity Parsing
mkOpExpr :: OpTree (Located Sugar) -> Located Sugar
mkOpExpr (Leaf e)   = e
mkOpExpr (Op i l r) = apply (pure i) [mkOpExpr l, mkOpExpr r]

parseError :: Lexeme -> Alex a
parseError l = alexError msg
  where
    msg = concat ["Parse Error, near ", fmtErr l]
}
