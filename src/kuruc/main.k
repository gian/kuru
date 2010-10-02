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
	
   val linepositions = ref [] : int list ref
   val p = ref 0

   fun readLines fp = 
      let
         val l = TextIO.inputLine fp
      in
         case l of NONE => ""
                 | SOME l' => (
                    linepositions := !linepositions @ [!p];
                    p := !p + size l';
                    l' ^ readLines fp)
      end


   val filename = hd args'

   val f = TextIO.openIn filename
   val buf = readLines f
   val _ = TextIO.closeIn f

   val p = KuruParser.kupeg_start buf handle Option => 
      raise Fail ("kuruc: error near " ^ 
                  (Error.resolveErrorPos(filename,
                                         !linepositions,
                                         KuruParser.error(),
                                         buf)))

   val _ = if KuruParser.error() + 20 < size buf then 
      print ("kuruc: maybe error near " ^ 
                  (Error.resolveErrorPos(filename,
                                         !linepositions,
                                         KuruParser.error(),
                                         buf)) ^ "\n") else ()


   val pretty = (String.concatWith "\n" (map ParseTree.prettyPrint p))

   fun main () = print (pretty ^ "\n")
end   
   
val _ = Main.main ()


