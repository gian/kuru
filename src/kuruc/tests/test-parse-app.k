fun cachefind c p =
  (fn NONE => NONE | SOME (k,v) => SOME v)
     (List.find (fn (p',v) => false) c)

