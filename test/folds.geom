define foldr(f, e, [])   = e
     | foldr(f, e, x:xs) = f(x, foldr(f, e, xs));

define foldl(f, e, [])   = e
     | foldl(f, e, x:xs) = foldl(f, f(e, x), xs);

define map(f, xs) =
  let app(x, ys) = f(x) : ys
  in foldr(app, [], xs);

define filter(p, xs) =
  let test(x, ys) = x:ys when p(x)
    | test(_, ys) = ys
  in foldr(test, [], xs);

define length(xs) =
  let plus1(x,_) = 1 + x
  in foldl(plus1, 0, xs);

define reverse(xs) =
  let snoc(y,x) = x:y
  in foldl(snoc, [], xs);
