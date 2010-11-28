fun f x = x 

fun g x y = x + y

fun h p 0 = 1
  | h p n = n + 1

fun i a b c 0 = a + b + c * 10
  | i a b c d = a - b - c - d

val rec i' = fn x1 => fn x2 => fn x3 => fn x4 =>
		(case (x1,x2,x3,x4) of (a,b,c,0) => a + b + c * 10
		                    | (a,b,c,d) => a - b - c - d)

