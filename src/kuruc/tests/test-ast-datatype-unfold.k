datatype foo = Foo of int | Bar of string | Baz

fun f Baz = 0
  | f (Bar _) = 10
  | f (Foo i) = i
