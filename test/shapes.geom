define PI = 3.14159;

define area([#rect, w, h]) = w * h
     | area([#square, s])  = s * s
     | area([#circle, r])  = r * r * PI;
