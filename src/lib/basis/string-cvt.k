structure StringCvt =
struct
   datatype realfmt = 
      SCI of int option
    | FIX of int option
    | GEN of int option
    | EXACT
end

