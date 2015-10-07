define . = function (g, f) function (x) g(f(x));
define add(x) = x + 1;
define add2 = add . add;
