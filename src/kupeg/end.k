
fun main () = 
	let
      val args = CommandLine.arguments ()

         fun parseArgs [] = []
           | parseArgs ("-v"::t) = parseArgs t
           | parseArgs (h::t) = h :: parseArgs t

         val args' = parseArgs args 

			val _ = if length args' < 1 then raise Fail "Usage: kupeg [-v] filename.kpg" else ()

			val filename = hd args'        
 
			val f = TextIO.openIn filename

			val buf = TextIO.input f
			val _ = TextIO.closeIn f

			val p = parse_sentence(buf,0)

			val p' = valOf (#va (valOf p))

			(* Generate the output file *)
			val fo = TextIO.openOut (filename ^ ".k")
			val _ = TextIO.output (fo, p')
			val _ = TextIO.closeOut fo
		in
			()	
      end

val _ = main ()

