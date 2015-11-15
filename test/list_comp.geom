{ Helper function for list comprehensions }
define _mapa(f, [], acc) = acc
  | _mapa(f, x:xs, acc) = f(x, _mapa(f, xs, acc));

{ Helper function for lists [a..b] }
define _range(a, b) = if a > b then [] else a:_range(a+1, b);

define a = 1;
define b = 10;
define xs = [["foo", "bar"]];
define ys = ["qux", "quux"];
define y = numeric(0);

[ x | x <- [a..b]];
[ x | [x, _] <- xs when y];
[ [x, y] | [x, _] <- xs, y <- ys];
