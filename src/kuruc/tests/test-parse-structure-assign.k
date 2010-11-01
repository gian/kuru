structure A =
struct

	val foo = 123
	val bar = "Bla"

	val _ = print "Hello, world "
end

structure B = A

val p = B.foo

val _ = print B.bar
