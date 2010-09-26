structure Kil =
struct

   fun repl () = 
      let
         fun prompt () = print " - "

         val ib = ref [] : char list ref

         val c = ref (SOME (Char.chr 0)) 

         val env = ref Interpret.top_level_env

         fun dstop NONE = false
           | dstop (SOME #";") = false
           | dstop _ = true

         fun readInput () = (while dstop (!c) do
                                (ib := !ib @ [valOf (!c)]; 
                                 c := TextIO.input1 TextIO.stdIn); 
                                 String.implode (!ib))
         fun replLoop () =
            let
               val _ = prompt ()
               val _ = c := (TextIO.input1 TextIO.stdIn)
               val i = readInput ()
               val _ = if i = "" then Unix.exit 0w0 else ()
               val p = KilParse.parse i 
               val env' = Interpret.interpret_h (!env) p
               val _ = env := env'
               val _ = ib := []
            in
               replLoop ()
            end
      in
         replLoop ()
      end
   

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
            val a = KilParse.parse t
            val _ = Debug.print 3 ("AST:\n" ^ (Ast.pp_l "\n" a) ^ "\n================\n")
         in
            Interpret.interpret a
         end
      end

end


val _ = Kil.main ()

