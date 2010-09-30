structure Main =
struct
   val args = CommandLine.arguments ()

   fun parseArgs [] = []
	  | parseArgs ("-v"::t) = (KuruParser.debugVerbose := true; 
                              parseArgs t)
     | parseArgs (h::t) = h :: parseArgs t
      
   val args' = parseArgs args 

   val _ = if length args' < 1 then 
            raise Fail "Usage: kuruc [-v] filename.k" else ()
		
   val filename = hd args'    
 
   val f = TextIO.openIn filename
   val buf = TextIO.input f
   val _ = TextIO.closeIn f

   val p = KuruParser.kupeg_start buf
   val pretty = ParseTree.prettyPrint p

   fun main () = print (pretty ^ "\n")
end   
   
val _ = Main.main ()


