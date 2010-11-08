signature FOO =
sig
	val p : int
	val q : string
	val f : int -> int
	val g : int -> int
   val h : 'a 
end

structure Foo : FOO =
struct
	val p = 123
	val q = "hello, world"
	
	fun f 0 = 0
	  | f x = x

	fun g x = x

	val h = int
end
