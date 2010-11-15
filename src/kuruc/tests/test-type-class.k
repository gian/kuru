class Foo of t =
sig
   val foo : t -> string
end

structure FooInt =
struct
   fun foo 0 = "I am an int"
end

structure FooChar =
struct
   fun foo #"c" = "I am a char"
end

type int implements Foo in FooInt
type char implements Foo in FooChar


val k = foo 0 
val j = foo #"c"
