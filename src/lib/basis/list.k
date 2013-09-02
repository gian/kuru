structure List =
struct

	fun find f [] = NONE
	  | find f (h::t) = if f h then SOME h else find f t

	fun map f [] = []
	  | map f (h::t) = f h :: map f t

   fun length [] = 0

   fun filter f [] = []

   fun exists f [] = false

   fun foldl f [] v = v
   fun foldr f [] v = v

   fun rev [] = []

   fun hd (h::t) = h
 
   fun tl (h::t) = t

   fun concat [] = []

   fun all f [] = false

   fun app f [] = ()

   fun nth (h::t, 0) = h
end

val map = List.map
val length = List.length
val hd = List.hd
val tl = List.tl

