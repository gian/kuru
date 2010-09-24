structure Kil =
struct

   fun repl () = (print "REPL not implemented!  Specify an input file\n")

   fun main () =
      let
         val args = CommandLine.arguments ()

         val _ = Debug.setDebugLevel 2
         
         fun parseArgs [] = []
           | parseArgs ("-v"::l::t) = (Debug.setDebugLevel (valOf (Int.fromString l));
                                       parseArgs t)
           | parseArgs ("-v"::nil) = raise (Fail "-v requires an integer debug level")
           | parseArgs (h::t) = h :: parseArgs t

         val args' = parseArgs args 

         val interactive = length (args') = 0
      in
         if interactive then repl () else 
         let
            val f = TextIO.openIn (hd args')
            val t = TextIO.input f
            val _ = TextIO.closeIn f
         in
            print (Ast.pp_l "\n" (KilParse.parse t))
         end
      end

end


val _ = Kil.main ()

