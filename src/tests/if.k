val t = 0 == 0

val f = 1 != 1

val t' = 0 < 1

val f' = 0 > 1

val t'' = ~1 <= 0

val f'' = ~1 >= 0

val _ = if t then (print "0 == 0") else (print "0 != 0")
val _ = if f then (print "1 != 1") else (print "1 == 1")
val _ = if t' then (print "0 < 1") else print "1 < 0"
val _ = if f' then (print "0 > 1") else print "1 > 0"
val _ = if t'' then (print "~1 <= 1") else print "1 <= ~1"
val _ = if f'' then (print "~1 >= 0") else print "0 >= ~1"

