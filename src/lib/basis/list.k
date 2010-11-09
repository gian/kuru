structure List =
struct
	fun all f [] = ()
	  | all f (h::t) = (f h; all f t)

	fun exists f [] = false
     | exists f (h::t) = f h orelse exists f t

   fun filter f [] = []
     | filter f (h::t) = if f h then h :: filter f t else filter f t

	fun find f [] = NONE
	  | find f (h::t) = if f h then SOME t else find f t

   fun foldl f b [] = b
     | foldl f b (h::t) = foldl f (f(h, b)) t

   fun foldr f b [] = b
     | foldr f b (h::t) = f(h, foldr f b t)

	fun length [] = 0
     | length (h::t) = 1 + length t
end
