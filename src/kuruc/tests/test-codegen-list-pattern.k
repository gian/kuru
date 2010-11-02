
val p = [1, 2, 3]

fun sum [] = 0
  | sum (h::t) = h * sum t


val q = sum p
