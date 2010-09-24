fun f x = x

val q = f 1

val _ = print ("f-app: " + q)

fun g x = fn y => x + y

val f' = g 10

val r = f' 32

val _ = print ("f'-app: " + r)

fun f'' x = (g x) x

val s = f'' 100

val _ = print ("f''-app: " + s)


fun f''' x y z = x + y + z

val t = f''' 1
val t' = t 10
val t'' = t' 100

val _ = print ("f'''-app: " + t'')

val t''' = t' 200

val _ = print ("f'''2-app: " + t''')

