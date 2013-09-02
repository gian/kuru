datatype foobar = A | B of unit | C of unit * unit

datatype baz = X | Y | Z

fun f A = X
  | f (B x) = Y
  | f (C (a,b)) = Z

(* exception Bar of string

fun g A = X
  | g _ = raise (Bar "foo")

fun h A = raise (Bar "baz")
  | h _ = 123
  *)

