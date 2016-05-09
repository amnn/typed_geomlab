{ Prelude }
define p2(x, y)    = function(f) f(x, y);
define p3(x, y, z) = function(f) f(x, y, z);

define ++ ([],   ys) = ys
     | ++ (x:xs, ys) = x : (xs ++ ys);

{ 1. Type error with lambda-bound variables used as function and integer }

function(f)
  function(a)
    p3(f(a), f(2), [f, 2]);

{ 2. Ground type conflict with lambda-bound variables }

function(f, c:cs, i:is)
  if   i > 0
  then f(cs, is)
  else (c : [2.2]) ++ f(is, cs);

{ 3. List type conflict with lambda-bound variables }

function(m, f, h:t)
  f(h) : f(t);

{ 4. Type error variable in location definition }

let f(x) = (let y = x in y(5)) in f(3);

{ 5. Type error with local definition with same variables in body }

function (x)
  let y(z) = (let a = x(z) in function(w) w)
  in p2(y(5), y(numeric(0)));

{ 6. Type error with local definition with partial polymorphic types }

function(f)
  let f(y) = y(x) in
  let fid  = f(function(z) z) in
    fid(f(function(u) function(v) u));
