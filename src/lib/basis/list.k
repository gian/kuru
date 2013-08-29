structure List =
struct

	fun find f [] = NONE
	  | find f (h::t) = if f h then SOME h else find f t

	fun map f [] = []
	  | map f (h::t) = f h :: map t
end

val map = List.map

