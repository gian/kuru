infix 7 *
infix 6 +

val p = [1, 2, 3]

fun sum [] = 0
  | sum (h::t) = h * sum t


fun foo [] = 0
  | foo [1,2,3,4] = 3
  | foo l = 1

val q = sum p

val words = ["three", "blind", "mice", "see", "how", "they", "run"]

fun sayWords [] = ()
  | sayWords (h::t) = (print h; print "\n"; sayWords t)

val _ = sayWords words


