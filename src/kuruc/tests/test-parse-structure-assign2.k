structure Foo = 
struct
	val p = "Hello, world\n"
end

structure Bar =
struct
	structure Baz = Foo
end

val p = Compiler.Internal.print Bar.Baz.p
