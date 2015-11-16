{ Helper functions for operator sections }
define _lsect(f, x) = function (y) f(x, y);
define _rsect(f, y) = function (x) f(x, y);

(:[]); (:["a"]); (1:); (#foo:);
