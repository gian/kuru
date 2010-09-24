fun f x = x

val q = f 1

val _ = print ("f-app: " + q)

fun g x = fn y => x + y

val f' = g 10

val r = f' 32

val _ = print ("X: " + x)

val _ = print ("f'-app: " + r)

fun f'' x = (g x) x

val s = f'' 100

val _ = print ("f''-app: " + s)


