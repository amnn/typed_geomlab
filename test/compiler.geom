{*
 * compiler.txt
 *
 * This file is part of GeomLab
 * Copyright (c) 2008 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *}

{ BASIC DEFINITIONS }

{ Some of these are redefined in the prelude with better error handling. }

define true = numeric(0);
define false = numeric(true);

define not (p) = if p then false else true;

define ++ ([], ys) = ys
  | ++ (x:xs, ys) = x:(xs ++ ys);

define concat([]) = []
  | concat(xs:xss) = xs ++ concat(xss);

define reverse(xs) =
  let reva([], vs) = vs | reva(u:us, vs) = reva(us, u:vs) in
  reva(xs, []);

define length([]) = 0
  | length(x:xs) = length(xs)+1;

define assoc(x, []) = []
  | assoc(x, [u,v]:zs) = if x = u then v else assoc(x, zs);

define map(f, []) = [] | map(f, x:xs) = f(x) : map(f, xs);

define _iter(f, []) = [] | _iter(f, x:xs) = f(x) >> _iter(f, xs);

define filter(p, []) = []
  | filter(p, x:xs) = if p(x) then x:filter(p, xs) else filter(p, xs);

define foldr(f, a, []) = a | foldr(f, a, x:xs) = f(x, foldr(f, a, xs));
define foldl(f, a, []) = a | foldl(f, a, x:xs) = foldl(f, f(a, x), xs);

{ Helper function for list comprehensions }
define _mapa(f, [], acc) = acc
  | _mapa(f, x:xs, acc) = f(x, _mapa(f, xs, acc));

{ Helper function for lists [a..b] }
define _range(a, b) = if a > b then [] else a:_range(a+1, b);

{ Helper functions for operator sections }
define _lsect(f, x) = function (y) f(x, y);
define _rsect(f, y) = function (x) f(x, y);


{ LEXICAL TOKENS }

_update(_syntax, #function, [#function, 0, 0]);

_iter(function (tok) _update(_syntax, tok, [tok, 0, 0]),
  [#define, #else, #if, #in, #let, #then, #when, #_, #.., #<-, #>>]);

_update(_syntax, #=, [#=, 3, 4]);
_update(_syntax, #+, [#+, 5, 6]);
_update(_syntax, #-, [#-, 5, 6]);
_update(_syntax, #:, [#:, 7, 7]);
_update(_syntax, #~, [#monop, 0, 0]);
_update(_syntax, #not, [#monop, 0, 0]);

define _infixl(tag, p) = _update(_syntax, tag, [#binop, p, p+1]);
define _infixr(tag, p) = _update(_syntax, tag, [#binop, p, p]);

_infixl(#or, 1);
_infixl(#and, 2);
_infixl(#<, 3); _infixl(#<=, 3); _infixl(#<>, 3);
_infixl(#>, 3); _infixl(#>=, 3);
_infixr(#++, 4);
_infixl(#^, 5);
_infixl(#*, 6); _infixl(#/, 6);


{ All definitions that are specific to the compiler are made local to it:
  this avoids 'polluting the name space' and makes bootstrapping easier.
  But the bootstrap loader does not support closures that have free variables,
  so here we create a function with no arguments and no free variables
  that (when it is called) builds a network of closures and returns the main
  compiling function.  Later (at the start of the prelude) we can call the
  no-argument function one last time and save the resulting closure. }

define __top() =
  let debug(n, x) = if _debug() > n then _print(x) else [] in

  let member(x, []) = false | member(x, y:ys) = (x = y) or member(x, ys) in

  let number(_, []) = []
    | number(n, x:xs) = [n, x] : number(n+1, xs) in

  let fst(x:_) = x in
  let snd(_:y:_) = y in

  let max(x, y) = if x > y then x else y in


  { PARSER -- a slightly hacked-up recursive descent parser.  The global
    variable tok contains the next token, and each parser routine p_thing
    recognises an instance of thing and returns the AST, expecting tok
    to contain the first token of the thing on entry, and leaving it with
    the first token after the thing.  The trickiness surrounds operator
    sections like (2*x+), where we discover that it is a section only after
    parsing the 2*x. }

  let synerror(tag) = _synerror(tag, []) in

  { describe -- string describing a token for error messages }
  let describe(#ident) = "an identifier"
    | describe(#number) = "a number"
    | describe(#atom) = "an atom"
    | describe(#lpar) = "'('"
    | describe(#rpar) = "')'"
    | describe(#comma) = "','"
    | describe(#semi) = "';'"
    | describe(#bra) = "'['"
    | describe(#ket) = "']'"
    | describe(#vbar) = "'|'"
    | describe(#>>) = "'>>'"
    | describe(#..) = "'..'"
    | describe(#string) = "a string constant"
    | describe(#binop) = "a binary operator"
    | describe(#monop) = "a unary operator"
    | describe(#lbrace) = "'{'"
    | describe(#rbrace) = "'}'"
    | describe(#eol) = "end of line"
    | describe(#eof) = "end of input"
    | describe(x) = "'" ^ _spelling(x) ^ "'" in

  { tok -- latest lexer token }
  let tok = _new(0) in

  { val -- value associated with latest token }
  let val = _new(0) in

  { scan -- call the lexer and set tok and val }
  let scan() =
    let t = _scan() in
    debug(2, t) >>
    let case(#ident) =
          let tk = _lookup(_syntax, snd(t)) in
          _set(tok, if tk <> [] then fst(tk) else #ident) >> debug(2, _get(tok))
      | case(#op) =
          let tk = _lookup(_syntax, snd(t)) in
          if tk <> [] then _set(tok, fst(tk)) else synerror("#badtok")
      | case(x) =
          _set(tok, x) in
    case(fst(t)) >> _set(val, snd(t)) in

  { priority -- find priority pair for operator }
  let _priority(op) =
    let tk = _lookup(_syntax, op) in
    if tk <> [] then tail(tk) else [0, 0] in

  let isbinop(t) = member(t, [#binop, #=, #-, #+, #:]) in

  { see -- test for possible token }
  let see(t) = (_get(tok) = t) in

  { eat -- match and consume token or report syntax error }
  let eat(t) =
    if see(t) then scan() else _synerror("#eat", [describe(t)]) in

  { can_eat -- match and consume token or return false }
  let can_eat(t) =
    if see(t) then (scan() >> true) else false in

  { A switch "case !tok of x -> ... | y -> ..." is rendered below as
    "let case(x) = ... | case(y) = ... in whichever(case)".  }

  { whichever -- case branch on next token }
  let whichever(case) = case(_get(tok)) in

  { p_sym -- match and consume a token and return its value }
  let p_sym(t) =
    let v = _get(val) in eat(t) >> v in

  { brack -- parse a phrase between brackets }
  let brack(open, p, close) =
    eat(open) >> let x = p() in eat(close) >> x in

  { brack1 -- parse phrase between brackets, passing close as argument }
  let brack1(open, p, close) =
    eat(open) >> let x = p(close) in eat(close) >> x in

  { p_tail -- parse tail of list with separator }
  let p_tail(p, sep) =
    if can_eat(sep) then (let e1 = p() in e1 : p_tail(p, sep)) else [] in

  { p_list1 -- parse non-empty list separated by commas }
  let p_list1(p) =
    let e1 = p() in e1 : p_tail(p, #comma) in

  { p_list -- parse optional list }
  let p_list(p, endtok) =
    if see(endtok) or see(#eof) then [] else p_list1(p) in

  { We allow mutual recursion by tying the knot with reference cells.
    Each cell made here has a function f that fetches the contents of
    the cell and calls it.  Later, we'll see a function f_body and an
    assignment _set(knot, f_body) that sets the cell. }
  let eknot = _new(0) in
  let p_expr0(secok) = let p = _get(eknot) in p(secok) in
  let p_expr() = p_expr0(false) in

  let tknot = _new(0) in
  let p_term(min, secok) = let p = _get(tknot) in p(min, secok) in

  let pknot = _new(0) in
  let p_pattern() = let p = _get(pknot) in p() in

  let dknot = _new(0) in
  let p_defn() = let p = _get(dknot) in p() in

  { listify -- form list expression or pattern }
  let listify(es) = foldr(function (h, t) [#cons, h, t], [#nil], es) in

  let p_patterns(endtok) = p_list(p_pattern, endtok) in

  let p_patprim() =
    let case(#ident) =
    	  let x = p_sym(#ident) in
	  if not see(#lpar) then [#var, x] else
	    #prim:x:brack1(#lpar, p_patterns, #rpar)
      | case(#atom) = [#const, p_sym(#atom)]
      | case(#_) = eat(#_) >> [#anon]
      | case(#number) = [#const, p_sym(#number)]
      | case(#-) = eat(#-) >> [#const, -p_sym(#number)]
      | case(#string) = [#const, p_sym(#string)]
      | case(#lpar) = brack(#lpar, p_pattern, #rpar)
      | case(#bra) = #list:brack1(#bra, p_patterns, #ket)
      | case(_) = synerror("#pattern") in
    whichever(case) in

  let p_patfactor() =
    let p = p_patprim() in
    if can_eat(#:) then [#cons, p, p_patfactor()] else p in

  let p_pattern_body() =
    let chain(p) =
      if can_eat(#+) then chain([#plus, p, p_sym(#number)]) else p in
    chain(p_patfactor()) in

  { p_formals -- parse formal parameters }
  let p_formals() = brack1(#lpar, p_patterns, #rpar) in

  { p_exprs -- parse optional list of expressions }
  let p_exprs(endtok) = p_list(p_expr, endtok) in

  { expand -- expand list comprehension into code with accumulator }
  let expand(e1, [], a) =
        { Base case: "[ e1 | ] ++ a" = e1:a }
        [#cons, e1, a]
    | expand(e1, [#gen, [#var, x], e2]:gs, a) =
        { Simple generator: "[ e1 | x <- e2, ... ] ++ a" =
	    _mapa(function (x, b) "[ e1 | ... ] ++ b", e2, a) }
        let b = [#var, _gensym()] in
	[#apply, [#var, #_mapa], [#function, 2,
	    [[[[#var, x], b], expand(e1, gs, b)]]], e2, a]
    | expand(e1, [#gen, patt, e2]:gs, a) =
        { Generator : "[ e1 | patt <- e2, ... ] ++ a" =
	    _mapa((function (patt, b) -> "[ e1 | gs ] ++ b" | (_, b) -> b),
                      e2, a) }
	let b = [#var, _gensym()] in
	[#apply, [#var, #_mapa], [#function, 2,
	    [[[patt, b], expand(e1, gs, b)], [[[#anon], b], b]]], e2, a]
    | expand(e1, [#when, e2]:gs, a) =
        { Test: "[ e1 | when e2, ...] ++ a" =
	    if e2 then "[e1 | ...] ++ a" else a }
	[#if, e2, expand(e1, gs, a), a] in

  let p_gen() =
    let p = p_pattern() in eat(#<-) >> [#gen, p, p_expr()] in

  { p_gens -- parse generators for a list comprehension }
  let p_gens() =
    let p_tail() =
      let case(#when) =
      	    eat(#when) >> let e = p_expr() in [#when, e] : p_tail()
        | case(#comma) =
	    eat(#comma) >> let g = p_gen() in g : p_tail()
	| case(_) = [] in
      whichever(case) in
    let g = p_gen() in g : p_tail() in

  { p_listexp -- parse contents of [ ... ] }
  let p_listexp() =
    if see(#ket) then
      { An empty list }
      [#nil]
    else
      (let e1 = p_expr() in
        let case(#comma) =
	      { A display of two or more items }
              #list:e1:p_tail(p_expr, #comma)
          | case(#..) =
	      { A range [e1 .. e2] }
              eat(#..) >> [#apply, [#var, #_range], e1, p_expr()]
          | case(#vbar) =
	      { A list comprehension }
	      eat(#vbar) >> expand(e1, p_gens(), [#nil])
          | case(_) =
	      { A singleton list }
 	      [#list, e1] in
        whichever(case)) in

  { p_parenexp -- parse expression after left paren }
  let p_parenexp() =
    if _get(tok) = #- or not isbinop(_get(tok)) then
      p_expr0(true)
    else
      (let w = p_sym(_get(tok)) in
        let prio = _priority(w) in
      	if see(#rpar) then
	  { An operator name (+) }
	  [#var, w]
        else
	  { A right section (+1) }
	  [#apply, [#var, #_rsect], [#var, w], p_term(snd(prio), false)]) in

  let p_primary() =
    let case(#number) = [#const, p_sym(#number)]
      | case(#atom) = [#const, p_sym(#atom)]
      | case(#string) = [#const, p_sym(#string)]
      | case(#ident) =
          let x = p_sym(#ident) in
	  if not see(#lpar) then [#var, x] else
	    #apply:[#var, x]:brack1(#lpar, p_exprs, #rpar)
      | case(#lpar) = brack(#lpar, p_parenexp, #rpar)
      | case(#bra) = brack(#bra, p_listexp, #ket)
      | case(#eof) = synerror("#exp")
      | case(_) = synerror("#badexp") in
    whichever(case) in

  let p_factor(secok) =
    let case(#monop) =
	  let w = p_sym(#monop) in
          if secok and see(#rpar) then
            [#var, w]
          else
            [#apply, [#var, w], p_factor(false)]
      | case(#-) =
	  eat(#-) >>
	  if see(#number) then
	    [#const, - p_sym(#number)]
	  else if secok and see(#rpar) then
            { (-) is the binary minus }
            [#var, #-]
          else
	    [#apply, [#var, #~], p_factor(false)]
      | case(_) = p_primary() in
    whichever(case) in

  { makebin -- create binary operator, treating 'and' and 'or' as special }
  let makebin(w, e1, e2) =
    let case(#and) = [#if, e1, e2, [#const, false]]
      | case(#or) = [#if, e1, [#const, true], e2]
      | case(_) = [#apply, [#var, w], e1, e2] in
    case(w) in

  { p_term -- parse a term containing operators with priority >= min.
      If secok is true, allow a left section (1+) or (-) }
  let p_term_body(min, secok) =
    { p_termcont -- loop to parse a sequence of operators and operands }
    let p_termcont(e1, min) =
      let t = _get(tok) in
      if not isbinop(t) then e1 else
	(let w = _get(val) in
	  let prio = _priority(w) in
	  if fst(prio) < min then e1 else
	    (eat(t) >>
	      if secok and see(#rpar) then
	        { A left section }
	        [#apply, [#var, #_lsect], [#var, w], e1]
              else
		{ Got an operator: look for its right operand }
	        (let e2 = p_term(snd(prio), false) in
		  { Continue by looking for the next operator }
		  p_termcont(makebin(w, e1, e2), min)))) in
    p_termcont(p_factor(secok), min) in

  { p_cond -- parse a conditional, maybe also allowing a left section }
  let p_cond(secok) =
    if can_eat(#if) then
      (let e1 = p_cond(false) in
        eat(#then) >> let e2 = p_cond(false) in
	eat(#else) >> let e3 = p_cond(false) in [#if, e1, e2, e3])
    else
      p_term(1, secok) in

  { p_expr -- parse an expression or perhaps a left section }
  let p_expr_body(secok) =
    let case(#let) =
	  eat(#let) >> let d = p_defn() in
	  eat(#in) >> [#let, d, p_expr()]
      | case(#function) =
	  eat(#function) >> let formals = p_formals() in
	  [#function, length(formals), [[formals, p_expr()]]]
      | case(_) =
	  let e = p_cond(secok) in
	  if can_eat(#>>) then [#seq, e, p_expr()] else e in
    whichever(case) in

  { p_name -- parse the name on the LHS of a definition }
  let p_name() =
    if isbinop(_get(tok)) or see(#monop) then
      p_sym(_get(tok))
    else
      p_sym(#ident) in

  { p_rhs -- parse right hand side of equation }
  let p_rhs(lhs) =
    eat(#=) >> let e = p_expr() in
    if can_eat(#when) then [lhs, p_expr(), e] else [lhs, e] in

  { p_rule -- parse one clause of a function definition }
  let p_rule(x, arity) =
    { Inlined p_name and p_sym for better error message }
    let y = _get(val) in
    let exp =
      if isbinop(_get(tok)) or see(#monop) then _get(tok) else #ident in
    if see(exp) and x <> y then synerror("#names") else [] >>
    eat(exp) >>
    let lhs = p_formals() in
    if length(lhs) = arity then [] else synerror("#arity") >>
    p_rhs(lhs) in

  { p_defn -- parse a definition }
  let p_defn_body() =
    let x = p_name() in
    if not see(#lpar) then
      (eat(#=) >> [#val, x, p_expr()])
    else
      (let lhs = p_formals() in
	let arity = length(lhs) in
	let rule = p_rhs(lhs) in
	[#fun, x, arity, rule :
	  p_tail(function () p_rule(x, arity), #vbar)]) in

  { p_para -- parse a top-level paragraph }
  let p_para() =
    if see(#eof) then #eof else
      (let p = if can_eat(#define) then p_defn() else p_expr() in
	if see(#rpar) then synerror("#parenmatch")
	else if see(#ket) then synerror("#bramatch")
	else if not see(#semi) and not see(#eof) then synerror("#junk")
	else [] >>
        p) in

  _set(eknot, p_expr_body) >>
  _set(tknot, p_term_body) >>
  _set(pknot, p_pattern_body) >>
  _set(dknot, p_defn_body) >>

  let parser() = scan() >> p_para() in


  { CODE LISTS -- The compiler puts together the object code as a tree,
    with instructions as the leaves, and internal nodes (marked with #seq)
    that signify concatenation of their children.  The function flatten
    makes the tree into a list in linear time. }

  { flatten -- arrange instruction tree into a list }
  let flatten(c) =
    let flat([], a) = a
      | flat(#seq:cs, a) = foldr(flat, a, cs)
      | flat(c, a) = c:a in
    flat(c, []) in

  let assemble(f, n, code) =
    { Fix up labels }
    let ltab = _hash() in
    let fixlab(lab) = _lookup(ltab, lab) in
    let pass1(n, [], a) = a
      | pass1(n, lab:code, a) =
          _update(ltab, lab, n) >> pass1(n, code, a) when numeric(lab)
      | pass1(n, inst:code, a) = pass1(n+1, code, inst:a) in
    let fixup([#JUMP, lab]) = [#JUMP, fixlab(lab)]
      | fixup([#JFALSE, lab]) = [#JFALSE, fixlab(lab)]
      | fixup([#TRAP, lab]) = [#TRAP, fixlab(lab)]
      | fixup(inst) = inst in
    let pass2(code) =
      foldl((function (a, inst) fixup(inst):a), [], code) in
    _assemble(f, n, pass2(pass1(0, code, []))) in


  { ENVIRONMENTS -- An environment is a 5-list [lev, arity, dict, fvs, size],
    where
      * lev is the integer level.
      * arity is the number of arguments of the current function.
      * dict is a cell containing an a-list of variables,
        each mapped to information needed to load it.  The triple
	[n, i, a] represents a definition at level n that requires
	the instruction [i, a] to load it.
      * fvs is a cell containing a list of free variables that will be
        present in the closure
      * size is a cell containing the current frame size
    For an inner function, fvs includes the name of the function as its
    first element.  Each closure has itself as the first free variable
    as a way of implementing local recursion. }

  let lookup(x, [_, _, dict, _, _]) = assoc(x, _get(dict)) in

  { empty -- empty environment }
  let empty() = [0, 0, _new([]), _new([]), _new(0)] in

  { newblock -- create new block for nested function }
  let newblock(f, arity, [lev, _, dict, _, _]) =
    let d = if f = "<function>" then [] else [[f, [lev+1, #FVAR, 0]]] in
    [lev+1, arity, _new(d++_get(dict)), _new([]), _new(0)] in

  { e_level -- get level of nesting }
  let e_level([lev, _, _, _, _]) = lev in

  { e_arity -- get arity }
  let e_arity([_, arity, _, _, _]) = arity in

  { e_fvars -- get list of free variables }
  let e_fvars([_, _, _, fvs, _]) = _get(fvs) in

  { e_size -- get size of local frame }
  let e_size([_, _, _, _, size]) = _get(size) in

  { inc_size -- adjust size of local frame }
  let inc_size([_, _, _, _, size], delta) =
    _set(size, _get(size)+delta) in

  { bind -- define name as local variable }
  let bind(x, i, a, [lev, _, dict, _, _]) =
    _set(dict, [x, [lev, i, a]] : _get(dict)) in

  { unbind -- remove local binding }
  let unbind(x, [_, _, dict, _, _]) =
    let h([y, _] : d) = d when x = y
      | h(v : d) = v : h(d)
      | h([]) = [] in
    _set(dict, h(_get(dict))) in

  { alloc -- allocate space in frame }
  let alloc(x, env) =
    let a = e_size(env) in
    bind(x, #LOCAL, a, env) >> inc_size(env, 1) >> a in

  { dealloc -- remove local variable and shrink frame }
  let dealloc(x, env) =
    unbind(x, env) >> inc_size(env, -1) in

  { alloc_fv -- allocate free variable slot }
  let alloc_fv(x, [_, _, _, fvs, _]) =
    let a = length(_get(fvs)) + 1 in
    _set(fvs, _get(fvs) ++ [x]) >> a in

  { islocal -- test if name is a local variable }
  let islocal(x, env) =
    let case([n, i, _]) = (n = e_level(env)) when i = #LOCAL or i = #ARG
      | case(_) = false in
    case(lookup(x, env)) in

  { selfrec -- test if name is a recursive call of the same function }
  let selfrec(x, env) =
    lookup(x, env) = [e_level(env), #FVAR, 0] in

  { reset -- delete local variables at end of clause }
  let reset([lev, _, dict, _, size]) =
    let h([_, [n, i, _]]) = (n < lev) when i = #LOCAL or i = #ARG
      | h(_) = true in
    _set(dict, filter(h, _get(dict))) >> _set(size, 0) in

  { CODE GENERATOR -- Translate AST into funcode }

  let labcount = _new(0) in

  let label() = _set(labcount, _get(labcount)+1) in

  { c_ref -- compile a variable reference }
  let c_ref(x, env) =
    let case([n, i, a]) =
    	  { x is a local or constant or known free variable }
          [i, a] when n = e_level(env) or i = #QUOTE
      | case([_, _, _]) =
          { x is local to an enclosing scope -- make it a free variable }
	  let a = alloc_fv(x, env) in
	  bind(x, #FVAR, a, env) >> [#FVAR, a]
      | case([]) =
          { x is not bound at all -- treat it as global }
	  [#GLOBAL, x] in
    case(lookup(x, env)) in

  { sortby -- sort a list by some measure }
  let sortby(f, xs) =
    let insert(x, []) = [x]
      | insert(x, y:ys) =
          if f(x) <= f(y) then x:y:ys
	  else y:insert(x, ys) in
    foldr(insert, [], xs) in

  { The pattern matching compiler c_patt returns a pair [code, traps] where
    code is code to match the pattern, and traps is a list of pairs
    [f, d] consisting of a failure label f and a stack depth d when
    that label is reached. The function pgen combines several such pairs into
    one pair corresponding to a compound pattern. }

  { pgen -- accumulate code for pattern matching }
  let pgen(root, kids, traps) =
      [[#seq, root, #seq:map(fst, kids)], traps ++ concat(map(snd, kids))] in

  { c_patt -- compile a pattern, assuming d+1 things on the stack }
  let c_patt([#const, v], d, env) =
        let f = label() in
	pgen([#seq, [#TRAP, f], [#QUOTE, v], [#MEQ]], [], [[f, d]])

    | c_patt([#var, x], d, env) =
        let f = label() in
        if islocal(x, env) then
	  pgen([#seq, [#TRAP, f], c_ref(x, env), [#MEQ]], [], [[f, d]])
        else
          pgen([#seq, [#BIND, alloc(x, env)]], [], [])

    | c_patt([#anon], d, env) = pgen([#POP], [], [])

    | c_patt(#prim:cn:args, d, env) =
        { A constructor pattern h(args) }
        let f = label() in
	let n = length(args) in
	pgen([#seq, [#TRAP, f], c_ref(cn, env), [#MPRIM, n]],
	     reverse([ c_patt(p1, d1, env) | [d1, p1] <- number(d, args) ]),
             [[f, d]])

    | c_patt([#cons, h, [#anon]], d, env) =
        { A cons pattern h : _ }
        let f = label() in
	pgen([#seq, [#TRAP, f], [#MCONS]],
             [c_patt(h, d+1, env), [[#POP], []]],
	     [[f, d]])

    | c_patt([#cons, h, t], d, env) =
        { A cons pattern h : t }
        let f = label() in
	pgen([#seq, [#TRAP, f], [#MCONS]],
             [c_patt(h, d+1, env), [[#GETTAIL], []], c_patt(t, d, env)],
	     [[f, d]])

    | c_patt([#nil], d, env) =
        { A nil pattern [] }
        let f = label() in
	pgen([#seq, [#TRAP, f], [#MNIL]], [], [[f, d]])

    | c_patt(#list:es, d, env) =
        c_patt(listify(es), d, env)

    | c_patt([#plus, p1, n], d, env) =
        { A plus pattern p + n }
        let f = label() in
	pgen([#seq, [#TRAP, f], [#MPLUS, n]],
	        [c_patt(p1, d, env)], [[f, d]]) in

  { c_arg -- compile code to match an argument }
  let c_arg(i, [#var, x], env) =
        { variable matches whole argument }
        bind(x, #ARG, i, env) >> pgen([], [], []) when not islocal(x, env)
    | c_arg(i, [#anon], env) =
        { anon matches whole argument }
	pgen([], [], [])
    | c_arg(i, p, env) =
        pgen([#seq, [#ARG, i]], [c_patt(p, 0, env)], []) in

  { c_match -- compile code to match a list of arguments }
  let c_match(ps, env) =
    { Carefully evaluate from left to right }
    let compile(_, []) = []
      | compile(i, p:patts) =
          let x = c_arg(i, p, env) in x : compile(i+1, patts) in
    let code = compile(0, ps) in
    [#seq:map(fst, code), concat(map(snd, code))] in

  { The functions c_rule and c_body are mutually recursive with c_exp.
    Since our language does not support mutual recursion for local
    functions, we fake it by tying the knot with a reference cell. }

  let knot = _new(0) in

  { c_rule -- compile code for one rule in a function }
  let c_rule([patts, body], env) =
        let c_exp = _get(knot) in
        let match = c_match(patts, env) in
        let eval = c_exp(body, env, true) in
        reset(env) >>
        [[#seq, fst(match), eval], snd(match)]

    | c_rule([patts, guard, body], env) =
        let f = label() in
        let c_exp = _get(knot) in
        let match = c_match(patts, env) in
        let test = c_exp(guard, env, false) in
        let eval = c_exp(body, env, true) in
        reset(env) >>
        [[#seq, fst(match), test, [#JFALSE, f], eval], [f, 0]:snd(match)] in

  { A list of traps is accumulated for the whole of a rule, each containing
    a label and the stack depth when control reaches it.  On the JVM, we
    have to pop all the junk from the stack explicitly, so we sort the
    traps in decreasing order of depth and intersperse the labels with
    the right number of POP instructions. }

  { c_traps -- compile popping code for traps }
  let c_traps(traps) =
    let h(d0, [], acc) = acc
      | h(d0, [f, d1]:ys, acc) =
         if d0 = d1 then h(d1, ys, f:acc)
	 else h(d0+1, [f, d1]:ys, [#POP]:acc) in
    #seq:h(0, traps, []) in

  { c_body -- compile code for a function body }
  let c_body([], env) = [#FAIL]
    | c_body(r:rs, env) =
	let rcode = c_rule(r, env) in
	let flabs = map(fst, snd(rcode)) in
	let traps = sortby(snd, snd(rcode)) in
        [#seq, fst(rcode), if traps = [] then [] else
	    [#seq, c_traps(traps), c_body(rs, env)]] in

  { c_closure -- compile code to form a closure }
  let c_closure(f, n, body, env) =
    let env1 = newblock(f, n, env) in
    let code = flatten(c_body(body, env1)) in
    let fvs = e_fvars(env1) in
    let nfvs = length(fvs) in
    debug(1, code) >>
    [#seq, [#QUOTE, assemble(f, n, code)], [#FRAME, nfvs+1],
      #seq:[ [#seq, c_ref(x, env), [#PUTARG, i]] | [i, x] <- number(1, fvs) ],
      [#CLOSURE, nfvs+1]] in

  { yield -- append RETURN instuction if needed }
  let yield(code, tl) =
    if tl then [#seq, code, [#RETURN]] else code in

  { c_exp -- compile code for an expression, including a RETURN if tl is true }
  let c_exp([#const, v], env, tl) = yield([#QUOTE, v], tl)

    | c_exp([#var, x], env, tl) = yield(c_ref(x, env), tl)

    | c_exp(#apply:[#var, f]:args, env, tl) =
        { Tail call to the same function }
	[#seq, #seq:[ c_exp(e, env, false) | e <- args ],
	  [#TCALL, length(args)]]
      when tl and selfrec(f, env) and length(args) = e_arity(env)

    | c_exp(#apply:f:args, env, tl) =
        { A general function call -- PREP and PUTARG provide hooks
	  for the back end to do inlining of primitives }
        let nargs = length(args) in
      	yield([#seq, c_exp(f, env, false), [#PREP, nargs],
  	    #seq:[ [#seq, c_exp(e, env, false), [#PUTARG, i]]
	    	     | [i, e] <- number(0, args) ],
	    [#CALL, nargs]], tl)

    | c_exp([#if, e1, e2, e3], env, tl) =
        let l1 = label() in let l2 = label() in
	if tl then
  	  [#seq, c_exp(e1, env, false), [#JFALSE, l1],
	    c_exp(e2, env, true), l1, c_exp(e3, env, true)]
	else
	  [#seq, c_exp(e1, env, false), [#JFALSE, l1],
	    c_exp(e2, env, false), [#JUMP, l2],
	    l1, c_exp(e3, env, false), l2]

    | c_exp([#let, [#val, x, [#const, v]], e2], env, tl) =
        { Special case: treat constants by substituting them }
        bind(x, #QUOTE, v, env) >>
        let c2 = c_exp(e2, env, tl) in
        unbind(x, env) >> c2

    | c_exp([#let, [#val, x, e1], e2], env, tl) =
        { Local value definition  let x = e1 in e2 }
	let c1 = c_exp(e1, env, false) in
        let a = alloc(x, env) in
        let c2 = c_exp(e2, env, tl) in
        dealloc(x, env) >> [#seq, c1, [#BIND, a], c2]

    | c_exp([#let, [#fun, f, n, rules], e2], env, tl) =
        { Local function definition }
	let c1 = c_closure(f, n, rules, env) in
        let a = alloc(f, env) in
        let c2 = c_exp(e2, env, tl) in
        dealloc(f, env) >>
	[#seq, c1, [#BIND, a], c2]

    | c_exp([#function, n, rules], env, tl) =
      	{ A lambda expression  function (patt_1, ..., patt_n) e1 ... }
	yield(c_closure("<function>", n, rules, env), tl)

    | c_exp([#cons, e1, e2], env, tl) =
        yield([#seq, c_exp(e1, env, false),
			c_exp(e2, env, false), [#CONS]], tl)

    | c_exp([#nil], env, tl) = yield([#NIL], tl)

    | c_exp(#list:es, env, tl) = c_exp(listify(es), env, tl)

    | c_exp([#seq, e1, e2], env, tl) =
        { Sequential composition e1 >> e2 }
	[#seq, c_exp(e1, env, false), [#POP], c_exp(e2, env, tl)] in

  _set(knot, c_exp) >>

  { i_func -- compile a function for the interpreter }
  let i_func(f, n, body, env) =
    let code = flatten(c_body(body, newblock(f, n, env))) in
    debug(1, code) >> _closure(assemble(f, n, code)) in

  { Function bodies are compiled, but expressions typed at the top-level
    prompt are evaluated by a little metacircular interpreter, which
    is itself compiled. }

  { interp -- interpret an expression, compiling any embedded functions }
  let interp([#const, v], env) = v
    | interp([#var, x], env) =
        let case([_, #QUOTE, v]) = v
	  | case([]) = _glodef(x) in
	case(lookup(x, env))
    | interp(#apply:f:args, env) =
        _apply(interp(f, env), [ interp(e, env) | e <- args ])
    | interp([#if, e1, e2, e3], env) =
        if interp(e1, env) then interp(e2, env) else interp(e3, env)
    | interp([#let, [#val, x, e1], e2], env) =
        bind(x, #QUOTE, interp(e1, env), env) >>
	let v = interp(e2, env) in
	unbind(x, env) >> v
    | interp([#let, [#fun, f, n, rules], e2], env) =
	bind(f, #QUOTE, i_func(f, n, rules, env), env) >>
	let v = interp(e2, env) in
	unbind(f, env) >> v
    | interp([#function, n, rules], env) =
	i_func("<function>", n, rules, env)
    | interp([#cons, e1, e2], env) =
        interp(e1, env) : interp(e2, env)
    | interp([#nil], env) =
        []
    | interp(#list:es, env) =
        [interp(e, env) | e <- es ]
    | interp([#seq, e1, e2], env) =
	interp(e1, env) >> interp(e2, env) in

  { exec -- execute a top-level phrase }
  let exec([#val, x, e]) =
        { A global value definition }
	_redefine(x) >>
	_topdef(x, interp(e, empty()))

    | exec([#fun, f, n, rules]) =
        { A global function definition }
	_redefine(f) >>
	_topdef(f, i_func(f, n, rules, empty()))

    | exec(exp) =
        { A top-level expression }
	_topval(interp(exp, empty())) in

  if _defined(#_syntax) then [] else _topdef(#_syntax, _hash()) >>

  { The read-eval-print routine }
  function ()
    let p = parser() in
    if p = #eof then false else
      (_toptext() >> debug(0, p) >>
        _set(labcount, 0) >> _setroot(interp) >>
	exec(p) >> true);

define _top() =
  let t = __top() in t();

{ After bootstrapping, redefine _top = __top() for efficiency }
