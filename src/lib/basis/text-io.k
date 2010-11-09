structure TextIO =
struct

	fun print s = (Compiler.Internal.print s; ())

end
