datatype 'a quux = Quux of 'a * 'a

datatype 'a foo =
   Bar of 'a * 'a foo
 | Baz of int * 'a bar
withtype 'a bar = 'a quux

val p = Baz (42, Quux ("hello", "goodbye"))


