structure TextIO =
struct

	fun output (f,s) = ()

	_import k_io_print : string -> cvoid as internal_print

	fun print s = (internal_print s; ())

	val stdErr = 0
end
