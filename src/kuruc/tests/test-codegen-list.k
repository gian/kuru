val p = [1, 2, 3, 4, 5]

val q = ["hello", "world"]

fun pr [] = ()
  | pr (h::t) = (print h; print "\n"; pr t)

val x = pr q

val _ = print "Happy days!\n"

