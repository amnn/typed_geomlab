define true = numeric(0);
define _rsect(f, y) = function (x) f(x, y);
define p(x, y) = function (f) f(x, y);

(+10);

"foo"+"bar";

if true then 1 else "foo";

let k(x, y) = x in k(1);

define id(x) = x;

define . = function(f, g) function (x) f(g(x));

define length([])   = 0
     | length(x:xs) = 1 + length(xs);

define f(j) = p(j(true), j(1));
f(function (x) x);

let j(x) = x in
  p(j(true), j(1));

define f(x) = f;
define g(y) = g(g);
define h(x) = p(h, h);
