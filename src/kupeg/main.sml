structure Kupeg =
struct

structure K = KupegParser

fun main () = 
   let
      val args = CommandLine.arguments ()

      fun parseArgs [] = []
	    | parseArgs ("-v"::t) = (K.debugVerbose := true; 
                                 parseArgs t)
        | parseArgs (h::t) = h :: parseArgs t
      
      val args' = parseArgs args 

      val _ = if length args' < 1 then raise Fail "Usage: kupeg [-v] filename.kpg" else ()
		
      val filename = hd args'    
 
      val startFn = ref ""
      val nameSy = ref ""
      val nontermTypes = ref [("char","string"),
                              ("literal","string")] 
                                : (string * string) list ref

      fun startSymbol s = startFn := 
        ("fun kupeg_start s =  valOf (parse_"^s^" (s,ref 0))\n") 

		fun nameSymbol s = nameSy := s

      fun genNontermType line =
        let
            val l' = String.explode line
            fun fof [] = []
              | fof ((#" ")::(#"o")::(#"f")::(#" ")::t) = []
              | fof (h::t) = h :: fof t
            val ntname = String.implode (fof l')
            val nttype = String.implode (List.drop (l',4 + size ntname))
            val _ = if ntname = "" then raise Fail (
                "missing name in %nonterm declaration") else ()
            val _ = if nttype = "" then raise Fail (
                "missing 'of' in %nonterm " ^ ntname) else ()
            val _ = nontermTypes := !nontermTypes @ [(ntname,nttype)]
         in
            ()
         end

      fun genNontermSymbols () =
        let
           fun f [] = ""
             | f ((n,d)::t) = "type res_" ^ n ^ " = " ^ d ^ " option\n"
                                ^ f t
        in
           f (!nontermTypes) ^ "\n"
        end

      fun readLines fp = 
         let
            val l = TextIO.inputLine fp
         in
            case l of NONE => ""
                    | SOME l' =>
                       if String.isPrefix "%%" l' then "" else
                       if String.isPrefix "%start " l' then 
                          (startSymbol 
                             (String.substring(l',7,size l' - 8)); 
                                readLines fp) else
                       if String.isPrefix "%name " l' then 
                          (nameSymbol 
                             (String.substring(l',6,size l' - 7)); 
                                readLines fp) else 
                       if String.isPrefix "%nonterm " l' then 
                          (genNontermType
                             (String.substring(l',9,size l' - 10)); 
                                readLines fp) else
                       if String.isPrefix "%" l' then readLines fp else
                       if l' = "" then "" else
                       l' ^ readLines fp
         end

      val f = TextIO.openIn filename
      val verbatim = readLines f
      val buf = readLines f

      val _ = TextIO.closeIn f
		
      val _ = if buf = "" then raise Fail "Empty body.  Possibly missing %%?" else ()
      val _ = if (!startFn) = "" then raise Fail "Empty start symbol. Missing %start?" else ()
      val _ = if (!nameSy) = "" then raise Fail "Empty name symbol. Missing %name?" else ()

      val p' = K.kupeg_start buf 

   fun gen (K.Rule (l,b)) = 
      "and parse_" ^ l ^ "(input,pos) : res_" ^ l ^  " =\n" ^
      "   let\n" ^
      "      val _ = debug_print \"parse_" ^ l ^ "\\n\"\n" ^ 
      "      val stack = ref [] : int list ref\n" ^
      "   in\n" ^
      "      " ^ gen b ^ "\n" ^
      "   end\n\n"
     | gen (K.Choice (s1,s2)) =
     "   let\n" ^
     "      val prestate = !pos\n" ^
     "      val opt1 = " ^ gen s1 ^ "\n" ^
     "   in\n" ^
     "      if notNone opt1 then opt1 else\n" ^
     "      (pos := prestate; " ^ gen s2 ^ ")\n" ^ 
     "   end\n"
     | gen (K.Sequence (s1, K.Null)) = gen s1
     | gen (K.Sequence (K.Label (l,e), s2)) =
     "   let\n" ^
     "      val prestate = !pos\n" ^
     "      val " ^ l ^ " = " ^ gen e ^ "\n" ^
     "   in\n" ^
     "      if notNone " ^ l ^  " then (" ^ gen s2 ^ ") else " ^
     "(pos := prestate;NONE)\n" ^
     "   end\n"
     | gen (K.Sequence (s1,s2)) =
     "   let\n" ^
     "      val prestate = !pos\n" ^ 
     "      val s1' = " ^ gen s1 ^ "\n" ^ 
     "   in\n" ^
     "      if notNone s1' then " ^ gen s2 ^ "   else (pos := prestate; NONE)\n" ^
     "   end\n"
     | gen (K.Literal t) = 
       "literal(input,pos,\"" ^ t ^ "\")"
     | gen (K.Nonterm l) = "parse_" ^ l ^ "(input,pos)"
     | gen (K.Negation t) =
     "   let\n" ^ 
     "      val prestate = !pos\n" ^ 
     "      val t = " ^ gen t ^ "\n" ^
     "      val _ = if notNone t then (pos := prestate) else ()\n" ^
     "   in\n" ^
     "      (fn NONE => SOME \"\" | SOME _ => NONE) t\n" ^
     "   end\n"
     | gen (K.Result t) = "SOME (" ^ t ^ ")"
     | gen (K.Null) = "SOME \"\""
     | gen _ = "*****Unimplemented!*******"

      val chlitdefs = 
      "fun $ f = valOf f\n\n" ^
      "val debugVerbose = ref false\n" ^ 
      "fun debug_print s = if (!debugVerbose) then print s else ()\n" ^
      "fun notNone (NONE : 'a option) = false | notNone (SOME _) = true\n" ^
      (!startFn) ^ "\n" ^ 
      "and parse_char(input, pos) = \n" ^
      "  if (!pos >= size input) then NONE else\n" ^
      "  (pos := !pos + 1; SOME (String.str (String.sub(input,(!pos - 1)))))\n" ^
      "  handle Subscript => NONE\n" ^ 
      "and literal(input, pos, str) = \n" ^
      "  (if (String.substring(input, !pos, size str) = str) then\n" ^
      "  (pos := !pos + size str; SOME  (str))\n" ^
      "  else NONE) handle Subscript => NONE\n\n" 
      

      val p'' = String.concatWith "\n" (map gen p')

      (* Generate the output file *)
			
      val fo = TextIO.openOut (filename ^ ".k")
      val _ = TextIO.output (fo, "(* Generated from " ^ filename ^ " *)\n\n")
      val _ = TextIO.output (fo, "structure " ^ !nameSy ^ " =\nstruct\n")
      val _ = TextIO.output (fo, verbatim ^ "\n")
      val _ = TextIO.output (fo, genNontermSymbols  ())
      val _ = TextIO.output (fo, chlitdefs)
      val _ = TextIO.output (fo, p'')
      val _ = TextIO.output (fo, "\nend\n")
      val _ = TextIO.closeOut fo
   in
      ()	
   end
end

val _ = Kupeg.main ()

