define . = function (f, g) function (x) f(g(x));
define p(x, y) = function (f) f(x, y);

let f(x) = p(x, x) in
let g = f . f . f . f in
let h = g . g . g . g in
h;
