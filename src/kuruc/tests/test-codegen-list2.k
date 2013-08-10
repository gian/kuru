val l = ["hello\n", "goodbye\n", "how are you\n"]

fun f [] = ()
  | f (h::t) = (print h; f t)

val _ = f l

