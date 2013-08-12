fun f 0 = g 1
  | f x = g x
and g 1 = 10
  | g y = f 0

val p = f 5

