
fun main () = 
   let
      val args = CommandLine.arguments ()

      fun parseArgs [] = []
	    | parseArgs ("-v"::t) = parseArgs t
        | parseArgs (h::t) = h :: parseArgs t
      
      val args' = parseArgs args 

      val _ = if length args' < 1 then raise Fail "Usage: kupeg [-v] filename.kpg" else ()
		
      val filename = hd args'    
      val _ = kpfileName := filename
 
      val startFn = ref ""
      val resultTy = ref ""
      val emptyVal = ref ""
      val nontermTypes = ref [] : (string * string) list ref

      fun startSymbol s = startFn := 
        ("fun kupeg_start s = " ^
            "(valOf (#va (valOf (parse_" ^ s ^ "(s,0))))) " ^
            "handle Option => raise Fail \"Parse failed.\"\n")

      fun resultSymbol s = resultTy := ("type kupeg_result = " ^ s ^ "\n")

      fun emptySymbol s = ()

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
             | f ((n,d)::t) = "type kupeg_result_" ^ n ^ " = " ^ d ^ "\n"
                                ^ f t
        in
            f (!nontermTypes)
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
                       if String.isPrefix "%empty " l' then 
                          (emptySymbol 
                             (String.substring(l',7,size l' - 7)); 
                                readLines fp) else
                       if String.isPrefix "%result " l' then 
                          (resultSymbol 
                             (String.substring(l',8,size l' - 9)); 
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
      val _ = if (!resultTy) = "" then raise Fail "Empty result type. Missing %result?" else ()

      val p' = kupeg_start buf 

      (* Generate the output file *)
			
      val fo = TextIO.openOut (filename ^ ".k")
      val _ = TextIO.output (fo, "(* Generated from " ^ filename ^ " *)\n\n")
      val _ = TextIO.output (fo, "type 'a st = { pos : int, " ^
                                 "va : 'a option }\n")
      val _ = TextIO.output (fo, "val $ = valOf\n") 
      val _ = TextIO.output (fo, genNontermSymbols ())
      val _ = TextIO.output (fo,
        "fun push (stack, s : 'a st option) = stack := s :: (!stack)"
     ^  "\n\n" 
     ^  "fun pop stack =\n" 
     ^  "let\n"
     ^  "   val s = !stack\n"
     ^  "   val _ = stack := tl s\n"
     ^  "in hd s end\n\n"

     ^  "fun pos_ (s : 'a st option ref) =\n"
     ^  "let\n"
     ^  "   val s' = valOf (!s)\n"
     ^  "in #pos s' end\n\n" 

     ^  "fun va_ (s : 'a st option ref) =\n"
     ^  "let\n"
     ^  "   val s' = valOf (!s)\n"
     ^  "in #va s' end\n\n"

     ^  "fun notnone s = case (!s) of NONE => false\n"
     ^  "                           | SOME x => true\n\n"

     ^  "fun kupeg_make_stack () = ref [] : 'a st option list ref\n")

      
      val _ = TextIO.output (fo, verbatim ^ "\n")
      val _ = TextIO.output (fo, !startFn)
      val _ = TextIO.output (fo, p')
      val _ = TextIO.closeOut fo
   in
      ()	
   end

val _ = main ()

