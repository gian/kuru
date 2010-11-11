structure Compiler =
struct
	structure Internal =
	struct
		_import k_io_print : string -> cvoid as print
   	_import k_int_to_string : int -> string as toString
		val true = Compiler.Internal.true 
		val false = Compiler.Internal.false
	end
end
