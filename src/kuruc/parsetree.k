structure ParseTree =
struct

   datatype pt =
      Int of bool * string
    | Real of bool * string * string * (bool * string) option
    | Char of string
    | String of string
    | Ident of string
    | LongId of pt * pt
    | Var of pt
    | Seq of pt * pt

   fun pp (Int (b,s)) = "Int [" ^ (if b then "~" else "") ^ s ^ "]" 
     | pp (Real (b,s1,s2,NONE)) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^ "]" 
     | pp (Real (b,s1,s2,SOME (b2,s3))) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^
         "e" ^ (if b2 then "~" else "") ^ s3 ^ "]"
     | pp (Char s) = "Char [#\"" ^ s ^ "\"]"
     | pp (String s) = "String [\"" ^ s ^ "\"]"
     | pp (Ident s) = "Ident [" ^ s ^ "]"
     | pp (Var p) = "Var [" ^ pp p ^ "]"
     | pp (LongId (l1,l2)) = "LongId [" ^ pp l1 ^ "." ^ pp l2 ^ "]"
     | pp (Seq (p1, p2)) = pp p1 ^ "\n" ^ pp p2

   fun prettyPrint p = pp p

end

