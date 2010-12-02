datatype bla = Foo of int
				 | Bar of string 
				 | Baz of bla

fun extract (Foo i) = (print (Int.toString i);())
  | extract (Bar s) = (print s;())
  | extract (Baz b) = extract b

val _ = extract (Baz (Foo 123))

val _ = print "\n"

val _ = extract (Bar "hello\n")
