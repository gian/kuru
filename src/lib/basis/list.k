structure List =
struct
	fun all f [] = ()
	  | all f (h::t) = (f h; all f t)

	fun exists f [] = false
     | exists f (h::t) = f h orelse exists f t

   fun filter f [] = []
     | filter f (h::t) = if f h then h :: filter f t else filter f t

	fun length [] = 0
     | length (h::t) = 1 + length t

	fun find f [] = NONE
	  | find f (h::t) = if f h then SOME h else find f t
end
