
fun main () = 
   let
      val args = CommandLine.arguments ()

      fun parseArgs [] = []
	    | parseArgs ("-v"::t) = parseArgs t
        | parseArgs (h::t) = h :: parseArgs t
      
      val args' = parseArgs args 

      val _ = if length args' < 1 then raise Fail "Usage: kupeg [-v] filename.kpg" else ()
		
      val filename = hd args'        
 
      val startFn = ref ""
      val resultTy = ref ""
      val emptyVal = ref ""

      fun startSymbol s = startFn := ("fun kupeg_start s = parse_" ^ s ^ "(s,0)\n")

      fun resultSymbol s = resultTy := ("type kupeg_result = " ^ s ^ "\n")

      fun emptySymbol s = emptyVal := ("val kupeg_empty = " ^ s ^ "\n")

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
                       if String.isPrefix "%empty " l' then 
                          (emptySymbol 
                             (String.substring(l',7,size l' - 7)); 
                                readLines fp) else
                       if String.isPrefix "%result " l' then 
                          (resultSymbol 
                             (String.substring(l',8,size l' - 9)); 
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
      val _ = if (!resultTy) = "" then raise Fail "Empty result type. Missing %result?" else ()
      val _ = if (!emptyVal) = "" then raise Fail "Empty empty value. Missing %empty?" else ()

      val p = kupeg_start buf 

      val p' = valOf (#va (valOf p))

      (* Generate the output file *)
			
      val fo = TextIO.openOut (filename ^ ".k")
      val _ = TextIO.output (fo, "(* Generated from " ^ filename ^ " *)\n\n")
      val _ = TextIO.output (fo, !resultTy)
      val _ = TextIO.output (fo, "type st = { pos : int, " ^
                                 "va : kupeg_result option }\n")
      
      val _ = TextIO.output (fo,
        "fun push (stack, s : st option) = stack := s :: (!stack)"
     ^  "\n\n" 
     ^  "fun pop stack =\n" 
     ^  "let\n"
     ^  "   val s = !stack\n"
     ^  "   val _ = stack := tl s\n"
     ^  "in hd s end\n\n"

     ^  "fun pos_ (s : st option ref) =\n"
     ^  "let\n"
     ^  "   val s' = valOf (!s)\n"
     ^  "in #pos s' end\n\n" 

     ^  "fun va_ (s : st option ref) =\n"
     ^  "let\n"
     ^  "   val s' = valOf (!s)\n"
     ^  "in valOf (#va s') end\n\n"

     ^  "fun notnone s = case (!s) of NONE => false\n"
     ^  "                           | SOME x => true\n\n"

     ^  "fun kupeg_make_stack () = ref [] : st option list ref\n")

      
      val _ = TextIO.output (fo, verbatim ^ "\n")
      val _ = TextIO.output (fo, !emptyVal)
      val _ = TextIO.output (fo, !startFn)
      val _ = TextIO.output (fo, p')
      val _ = TextIO.closeOut fo
   in
      ()	
   end

val _ = main ()

