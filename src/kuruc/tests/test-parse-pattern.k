datatype foobar =
   REAL of int * string * foobar
 | FOO of string
 | NOTHING
 | NAME of int

fun pp (REAL (b,s1,FOO s2)) = s2
  | pp NOTHING = "Nothing at all"

fun substinty (NAME s) th (NAME t) = if true then th else NAME t

