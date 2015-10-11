[ x | x <- [a..b]];
[ x | [x, _] <- xs when y];
[ [x, y] | [x, _] <- xs, y <- ys];
