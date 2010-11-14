fun f 0 = 0
  | f x = x

val p = 1 + 2 + f 3 + 4 + 5 + 6 

val q = 1 :: 2 :: 3 :: f 4 :: 5 :: []
