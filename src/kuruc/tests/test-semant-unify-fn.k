fun f g x = g x "hello"

val p = f (fn x => x)

val q = p (fn x => 10)

fun id x = x

val y = id "hello"

val z = id 0

