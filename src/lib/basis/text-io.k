structure TextIO =
struct

	fun output (f,s) = ()

	fun print s = (Compiler_Internal.print s; ())

	val stdErr = 0
end
