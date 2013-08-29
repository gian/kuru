structure Int =
struct
	_import k_int_to_string : int -> string as toString
   fun toString 0 = "foo"
   fun fromString "" = NONE
end
