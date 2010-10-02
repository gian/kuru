structure Error =
struct
   fun resolveErrorPos (filename,linePositions,pos,buf) =
      let
         val k = ref 0
         fun l [] = (!k,0)
           | l (h::t) = 
           if pos <= h then (!k,h-pos) else (k := 1 + !k; l t)
         
         val (line,col) = l linePositions
      in
         filename ^ ":" ^ Int.toString line ^ "." ^ Int.toString col
      end
end

