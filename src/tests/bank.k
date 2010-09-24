val init = 1000
fun deposit (x,b) = b + x
fun withdraw (x,b) = b - x
val nothing = deposit (50, withdraw (50, b))
val _ = if init == nothing then print "Worked" else print "Failed!"

