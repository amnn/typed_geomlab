{
{-# LANGUAGE PatternGuards #-}
module GLParser where

import Lexer
import Data.Literal
import Data.Location
import Data.Patt
import Data.Sugar
import Data.Token

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
     | Expr ';'        { Eval . dislocate . reify "expression" $ $1 }

Decl ::            { Located Decl }
Decl : Id '=' Expr { Decl <@> $1 <*> reify ("definition of \'" ++ dislocate $1 ++ "\'") $3 }
     | FnBody      {% fnToDecl $ (reverse $1) }

FnBody ::                 { [Located FnArm] }
FnBody : FnArm            { [$1] }
       | FnBody '|' FnArm { $3 : $1 }

FnArm ::                         { Located FnArm }
FnArm : FnLhs '=' Expr           { mkFnArm $1 $3 (pure Nothing) }
      | FnLhs '=' Expr when Expr { mkFnArm $1 $3 (Just <@> (reify "guard" $5)) }

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
     | let Decl in Expr      { reify "let expression" $ $1 *> declToLet $2 (reify "body" $4) }
     | function Formals Expr { $1 *> anonFn $2 $3 }

ExprOrSect ::                      { Located Sugar }
ExprOrSect : let Decl in Expr      { reify "let expression" $
                                       $1 *> declToLet $2 (reify "body" $4)
                                   }

           | function Formals Expr { $1 *> anonFn $2 $3 }
           | CondOrSect            { $1 }

Cond ::                            { Located Sugar }
Cond : Term                        { $1 }
     | if Cond then Cond else Cond { reify "if expression" $ $1 *> reifyIf $2 $4 $6 }

CondOrSect ::                            { Located Sugar }
CondOrSect : TermOrSect                  { $1 }
           | if Cond then Cond else Cond { reify "if expression" $ $1 *> reifyIf $2 $4 $6 }

Term ::       { Located Sugar }
Term : Factor { $1 }
     | OpTree { mkOpExpr $1 }

TermOrSect ::             { Located Sugar }
TermOrSect : Factor       { $1 }
           | Factor BinOp { LSectS <@> reify "operand" $1 <*> $2 }
           | OpTree       { mkOpExpr $1 }
           | OpTree BinOp { LSectS <@> reify "operand" (mkOpExpr $1) <*> $2 }

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
       | monop Factor { reify "function application" $ apply (ident $1) [$2] }
       | '-' Factor   { reify "function application" $ apply ($1 *> pure "~") [$2] }

Primary ::                          { Located Sugar }
Primary : num                       { val $1 }
        | atom                      { val $1 }
        | str                       { val $1 }
        | ident                     { val $1 }

        | ident '(' Actuals ')'     { reify "function application" $ apply (ident $1) (reverse $3) <* $4 }

        | '[' ListExpr ']'          { reify (fst $2) $ $1 *> snd $2 <* $3 }
        | '(' monop ')'             { $1 *> val $2 <* $3 }
        | '(' BinOp ')'             { $1 *> (VarS <@> $2) <* $3 }

        | '(' BinOpNoMinus Term ')' { reify "right section" $
                                        $1 *> RSectS <@> $2 <*> reify "operand" $3 <* $4
                                    }

        | '(' ExprOrSect ')'        { let expr = $1 *> $2 <* $3
                                      in if isLSect (dislocate expr)
                                      then reify "left section" $ expr
                                      else expr
                                    }

Actuals ::                 { [Located Sugar] }
Actuals : {- empty -}      { [] }
        | Expr             { [$1] }
        | Actuals ',' Expr { $3 : $1 }

ListExpr ::               { ( String, Located Sugar) }
ListExpr : Actuals        { ( "list", reifyList $1) }

         | Expr '..' Expr { ( "range"
                            , RangeS <@> reify "lowerbound" $1 <*> reify "upperbound" $3
                            ) }

         | Expr '|' Gens  { ( "list comprehension"
                            , ListCompS <@> reify "yield" $1 <*> loc (reverse $3)
                            ) }

Gens ::                        { [Located Gen] }
Gens : Patt '<-' Expr          { [GenB <@> $1 <*> reify "generator" $3] }
     | Gens ',' Patt '<-' Expr { (GenB <@> $3 <*> reify "generator" $5) : $1 }
     | Gens when Expr          { (FilterB <@> reify "guard" $3) : $1 }

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
reify :: String -> Located Sugar -> Located Sugar
reify lbl ls@(L s _) = L s (LocS lbl ls)

reifyIf :: Located Sugar
        -- ^ Condition Expression
        -> Located Sugar
        -- ^ Then Expression
        -> Located Sugar
        -- ^ Else Expression
        -> Located Sugar

reifyIf lc lt le = IfS <@> reify "condition"   lc
                       <*> reify "then branch" lt
                       <*> reify "else branch" le

reifyOrd :: String -> Int -> Located Sugar -> Located Sugar
reifyOrd lbl i = reify (ordinal i ++ " " ++ lbl)
  where
    ordinal n =
      let sn   = show n
          lsd  = n `mod` 10
          lsd2 = n `mod` 100
      in case lsd of
        1 | lsd2 /= 11 -> sn ++ "st"
        2 | lsd2 /= 12 -> sn ++ "nd"
        3 | lsd2 /= 13 -> sn ++ "rd"
        _              -> sn ++ "th"

reifyList :: [Located Sugar] -> Located Sugar
reifyList [ls] = enlist <@> loc [reify "element" ls]
reifyList lss  = enlist <@> loc (zipWith (reifyOrd "element") [s,s-1..1] lss)
  where
    s = length lss

apply :: Located Id -> [Located Sugar] -> Located Sugar
apply x [y] = AppS <@> x <*> loc [reify "argument" y]
apply x xs  = AppS <@> x <*> loc (zipWith (reifyOrd "argument") [1..] xs)

isLSect :: Sugar -> Bool
isLSect (LSectS _ _)     = True
isLSect (LocS _ (L _ s)) = isLSect s
isLSect _                = False


-- Projections
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

-- Function Declarations
mkFnArm :: (Located Id, Located [Patt])
        -- ^ Function Interface
        -> Located Sugar
        -- ^ Function Body
        -> Located (Maybe Sugar)
        -- ^ Optional guard condition
        -> Located FnArm

mkFnArm (li, lps) lb lg = FnArm <@> li <*> lps <*> reify label lb <*> lg
  where
    label = "body of \'" ++ dislocate li ++ "\'"

fnToDecl :: [Located FnArm] -> Alex (Located Decl)
fnToDecl body@(a:as)
  | (b:_) <- checkMatch name  = alexError $ mismatch name  "name"  b
  | (b:_) <- checkMatch arity = alexError $ mismatch arity "arity" b
  | otherwise                 = return (Decl fnName <@> reify label (FnS <@> loc body))
  where
    name  (FnArm x _ _ _) = x
    arity (FnArm _ fs _ _) = length fs

    fnName = name . dislocate $ a
    label  = "definition of '" ++ fnName ++ "'"

    checkMatch proj =
      let lproj = proj . dislocate
      in  filter ((lproj a /=) . lproj) as

    mismatch proj label b =
      concat [ "Parse Error, function ", label
             , " mismatch"
             , "\nbetween ", fmtErr (proj <$> a)
             , "\n    and ", fmtErr (proj <$> b)
             , "."
             ]

fnToDecl [] = error "fnToDecl: Empty function definition."

anonFn :: Located [Patt] -> Located Sugar -> Located Sugar
anonFn lps lb = FnS <@> loc [FnArm "" <@> lps <*> reify "function body" lb <*> pure Nothing]

-- Operator Associativity Parsing
mkOpExpr :: OpTree (Located Sugar) -> Located Sugar
mkOpExpr (Leaf e)   = e
mkOpExpr (Op i l r) = reify "function application" $ apply (pure i) [mkOpExpr l, mkOpExpr r]

parseError :: Lexeme -> Alex a
parseError l = alexError msg
  where
    msg = concat ["Parse Error, near ", fmtErr l]
}
