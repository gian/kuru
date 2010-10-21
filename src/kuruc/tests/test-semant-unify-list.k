val l = [1, 2, 3, 4]

fun f [] = []
  | f (h::t) = f t

val k = f l
