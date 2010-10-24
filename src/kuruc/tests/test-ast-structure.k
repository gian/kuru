structure Foo = 
struct
	val p = 10
	val q = "hello, world"

	structure Bar =
	struct
		val p = "hello, world"
		val z = 123
		val z' = z
		val q' = q
	end

end


val k = Foo.p

val j = Foo.q

val q = k

val r = Foo.Bar.p
val s = Foo.Bar.z
