signature FOO =
sig
	val p : int
	val q : string
	val f : int -> int
	val g : int -> int
end

structure Foo : FOO =
struct
	val p = 123
	val q = "hello, world"
	
	fun f 0 = 0
	  | f x = x

	fun g 0 = 0
	  | g 1 = 1
	  | g x = x

	val h = 72 
end
