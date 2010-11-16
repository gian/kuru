class Foo of t =
sig
   val foo : t -> int 
end

structure FooInt =
struct
   fun foo 0 = 0
	  | foo x = 123
end

structure FooChar =
struct
   fun foo #"c" = 3
	  | foo _ = 321
end

type int implements Foo in FooInt
type char implements Foo in FooChar


val k = foo 0 
val j = foo #"c"

val _ = print (Int.toString k)
val _ = print "\n"
val _ = print (Int.toString j)
val _ = print "\n"

