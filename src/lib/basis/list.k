structure List =
struct

	fun find f [] = NONE
	  | find f (h::t) = if f h then SOME h else find f t

end
