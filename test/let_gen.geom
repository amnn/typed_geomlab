define id(x) = x;

let i = id(id) in i(1);

let f(x) = x in
let g = f(f) in
  f(#foo);
