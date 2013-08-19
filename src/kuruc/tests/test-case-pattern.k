datatype foobar = A | B of unit | C of unit * unit

datatype baz = X | Y | Z

fun f A = X
  | f (B x) = Y
  | f (C (a,b)) = Z


