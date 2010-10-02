structure ParseTree =
struct

   datatype pt =
      Int of bool * string
    | Real of bool * string * string * (bool * string) option
	
   fun pp (Int (b,s)) = "Int [" ^ (if b then "~" else "") ^ s ^ "]" 
	     | pp (Real (b,s1,s2,NONE)) = 
		        "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^ "]" 

   fun prettyPrint p = pp p



end

