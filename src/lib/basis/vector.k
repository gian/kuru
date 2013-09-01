structure Vector =
struct
   datatype 'a vector = Vector of 'a

	fun foldr f b x = b
   fun foldl f b x = b
   fun foldli f b x = b
   fun map f [] = []
   fun fromList (h::t) = Vector h
   fun tabulate (0, f) = Vector (f 0)
   fun sub (Vector i, 0) = i
   fun update (Vector i, 0, v) = Vector v
end
