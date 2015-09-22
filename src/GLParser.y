{
module GLParser where
import Lexer
}

%name        parseExpr Top
%error     { parseError }
%lexer     { getToken } { L Eof l c }
%monad     { Alex }
%tokentype { Lexeme }

%token '('    { L LPar l c }
       ')'    { L RPar l c }
       '['    { L Bra l c }
       ']'    { L Ket l c }
       ','    { L Comma l c }
       ';'    { L Semi l c }
       '|'    { L VBar l c }
       num    { L (Num   $$) l c }
       str    { L (Str   $$) l c }
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
     | FnBody         { fnToDecl (reverse $1) }

FnBody ::                 { [FnArm] }
FnBody : FnArm            { [$1] }
       | FnBody '|' FnArm { $3 : $1 }

FnArm ::                                 { FnArm }
FnArm : ident Formals '=' Expr           { MatchA $1 $2 $4 }
      | ident Formals '=' Expr when Expr { CondA  $1 $2 $4 $6 }

Expr : num { numS $1 }

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
data Shape a = NumS Double
             | StrS String
             | NilS
             | AtomS Id
             | ConsS a a
               deriving (Eq, Show)

class HasShape a where
  embedShape :: Shape a -> a

numS :: HasShape a => Double -> a
numS = embedShape . NumS

strS :: HasShape a => String -> a
strS = embedShape . StrS

nilS :: HasShape a => a
nilS = embedShape NilS

atomS :: HasShape a => Id -> a
atomS = embedShape . AtomS

consS :: HasShape a => a -> a -> a
consS x xs = embedShape (ConsS x xs)

enlist, enlist1 :: HasShape a => [a] -> a
enlist  = foldl  (flip consS) nilS
enlist1 = foldl1 (flip consS)

data Patt  = ValP (Shape Patt)
           | AnonP
           | VarP Id
           | OffsetP Patt Double
             deriving (Eq, Show)

instance HasShape Patt where
  embedShape = ValP

data FnArm = MatchA Id [Patt] Expr
           | CondA  Id [Patt] Expr Expr
             deriving (Eq, Show)

data Decl = Decl Id Expr
            deriving (Eq, Show)

data Expr = LitE (Shape Expr)
          | VarE Id
          | IfE Expr Expr Expr
          | FnE [FnArm]
          | AppE Expr [Expr]
          | LetE Id Expr Expr
          | SeqE [Expr]
            deriving (Eq, Show)

instance HasShape Expr where
  embedShape = LitE

data Para = Def Id Expr
          | Eval Expr
            deriving (Eq, Show)

-- TODO: This should validate the funciton (bigger than 1, all names, arities the same, etc)
fnToDecl :: [FnArm] -> Decl
fnToDecl body@((MatchA id _ _):_)   = Decl id (FnE body)
fnToDecl body@((CondA  id _ _ _):_) = Decl id (FnE body)

declToDef :: Decl -> Para
declToDef (Decl id e) = Def id e

declToLet :: Decl -> Expr -> Expr
declToLet (Decl id e) = LetE id e

parseError :: Lexeme -> Alex a
parseError (L tok l c) = alexError msg
  where
    msg = concat ["Parse Error, near ", show tok
                 , " at line ", show l, ", column ", show c, "."]

parse :: String -> [Para]
parse input = either error id (runAlex input parseExpr)
}
