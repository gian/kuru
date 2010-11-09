structure Foo =
struct
	val p = 123
	val q = "abc\n"
	type t = int
end

structure Bar =
struct
	open Foo

	val _ = print q
end
