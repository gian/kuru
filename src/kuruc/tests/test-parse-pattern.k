datatype foobar =
   REAL of int * string * foobar
 | FOO of string
 | NOTHING

fun pp (REAL (b,s1,FOO s2)) = s2
  | pp (NOTHING) = "Nothing at all"
