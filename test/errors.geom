define _rsect(f, y) = function (x) f(x, y);

{ Type Error in Operator Section }
(+1+"foo");

{ Type Error In If }
if numeric(0) then 1 + "foo" else 0;

{ Different Types in If Branches }
if numeric(0) then 1 else "foo";

{ Unbound Variable }
1 + foo;

{ Arity Mismatch }
let k(x, y) = x in k(1);

{ Errors in pattern matching }
define foo([1])     = true
     | foo(["foo"]) = false;

{ Infinite Types }
define k(x) = k;
define f(y) = f(f);
