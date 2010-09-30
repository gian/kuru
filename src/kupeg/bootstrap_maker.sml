(* Generated from kupeg.kpg *)

datatype ast =
   Rule of string * ast
 | Choice of ast * ast
 | Sequence of ast * ast
 | Literal of string
 | Nonterm of string
 | Negation of ast
 | Result of string
 | Label of string * ast
 | Star of ast
 | Plus of ast
 | Null

fun gen (Rule (l,b)) = 
   "and parse_" ^ l ^ "(input,pos) : res_" ^ l ^  " =\n" ^
   "   let\n" ^
   "      val _ = print \"print_" ^ l ^ "\\n\"\n" ^ 
   "      val stack = ref [] : int list ref\n" ^
   "   in\n" ^
   "      " ^ gen b ^ "\n" ^
   "   end\n\n"
  | gen (Choice (s1,s2)) =
  "   let\n" ^
  "      val prestate = !pos\n" ^
  "      val opt1 = " ^ gen s1 ^ "\n" ^
  "   in\n" ^
  "      if notNone opt1 then opt1 else\n" ^
  "      (pos := prestate; " ^ gen s2 ^ ")\n" ^ 
  "   end\n"
  | gen (Sequence (s1, Null)) = gen s1
  | gen (Sequence (Label (l,e), s2)) =
  "   let\n" ^
  "      val prestate = !pos\n" ^
  "      val " ^ l ^ " = " ^ gen e ^ "\n" ^
  "   in\n" ^
  "      if notNone " ^ l ^  " then (" ^ gen s2 ^ ") else " ^
  "(pos := prestate;NONE)\n" ^
  "   end\n"
  | gen (Sequence (s1,s2)) =
  "   let\n" ^
  "      val prestate = !pos\n" ^ 
  "      val s1' = " ^ gen s1 ^ "\n" ^ 
  "   in\n" ^
  "      if notNone s1' then " ^ gen s2 ^ "   else (pos := prestate; NONE)\n" ^
  "   end\n"
  | gen (Literal t) = 
    "literal(input,pos,\"" ^ t ^ "\")"
  | gen (Nonterm l) = "parse_" ^ l ^ "(input,pos)"
  | gen (Negation t) =
  "   let\n" ^ 
  "      val prestate = !pos\n" ^ 
  "      val t = " ^ gen t ^ "\n" ^
  "      val _ = if notNone t then (pos := prestate) else ()\n" ^
  "   in\n" ^
  "      (fn NONE => SOME \"\" | SOME _ => NONE) t\n" ^
  "   end\n"
  | gen (Result t) = "SOME (" ^ t ^ ")"
  | gen (Null) = "SOME \"\""
  | gen _ = "*****Unimplemented!*******"

val parser =
[Rule ("sp",
   Choice (Sequence(Literal " ", Null),
      Choice (Sequence(Literal "\\n", Result(("\"\\n\""))),
         Sequence(Literal "\\t", Null)))),


Rule ("_",
   Choice (Sequence(Nonterm "sp", 
           Sequence(Nonterm "_", Null)),Null)),

Rule ("rule",
   Sequence(Label ("n",Nonterm "name"), 
      Sequence(Nonterm "_", 
         Sequence(Literal "<-", 
            Sequence(Nonterm "_", 
               Sequence(Label ("body",Nonterm "choice"), 
                  Sequence(Literal ".", 
                     Sequence(Nonterm "_", 
                        Result("Rule($n,$body)"))))))))),

Rule ("sentence",
   Choice (
      Sequence(Nonterm "_", 
         Sequence(Label ("r",Nonterm "rule"), 
            Sequence(Label ("g",Nonterm "sentence"), 
               Result(("$r :: $g"))))),
      Sequence(Nonterm "_", 
         Sequence(Label ("r",Nonterm "rule"), 
            Result(("$r :: []")))))),

Rule ("meta",
   Choice (Sequence(Literal "!", Null),
      Choice (Sequence(Literal "\\\"", Null),
         Choice (Sequence(Literal "<-", Null),
            Choice (Sequence(Literal "/", Null),
               Choice (Sequence(Literal ".", Null),
                  Choice (Sequence(Literal "(", Null),
                     Choice (Sequence(Literal ")", Null),
                        Choice (Sequence(Literal ":", Null),
                           Choice (Sequence(Literal "->", Null),
                              Choice (Sequence(Literal "*", Null),
                                 Sequence(Literal "+", Null)))))))))))),


Rule ("name",
   Choice (Sequence(Label ("c",Nonterm "namechar"), 
      Sequence(Label ("n",Nonterm "name"),
         Result(("$c ^ $n")))),
            Sequence(Nonterm "namechar", Null))),


Rule ("namechar",
   Sequence(Negation(Nonterm "meta"), 
      Sequence(Negation(Nonterm "sp"), 
         Sequence(Nonterm "char", Null)))),

Rule ("termfrag",
   Choice (Sequence(Nonterm "labeled", Null),
      Choice (Sequence(Nonterm "nonterminal", Null),
         Choice (Sequence(Nonterm "string", Null),
            Choice (Sequence(Nonterm "negation", Null),
               Sequence(Nonterm "parenthesized", Null)))))),


Rule ("term",
   Sequence(Label ("t",Nonterm "termfrag"),
      Sequence(
         Choice (
         Sequence(Literal "*", Result("(Star ($t))")),
         Choice (
         Sequence(Literal "+", Result("(Plus ($t))")),
                               Result("($t)"))), Null))),


Rule ("nonterminal",
   Sequence(Label ("n",Nonterm "name"), 
      Sequence(Nonterm "_", Result("Nonterm ($n)")))),


Rule ("labeled",
   Sequence(Label ("label",Nonterm "name"), 
      Sequence(Nonterm "_", 
         Sequence(Literal ":", 
            Sequence(Nonterm "_", 
               Sequence(Label ("value",Nonterm "term"), 
                  Result("Label($label,$value)"))))))),


Rule ("sequence",
   Choice (
      Sequence(Label ("foo",Nonterm "term"), 
         Sequence(Label ("bar",Nonterm "sequence"), 
            Result("Sequence($foo, $bar)"))),
   Choice (
      Sequence(Nonterm "result_expression", Null),Result(("Null"))))),


Rule ("string",
   Sequence(Literal "\\\"", 
      Sequence(Label ("s",Nonterm "stringcontents"), 
         Sequence(Literal "\\\"", 
            Sequence(Nonterm "_", Result("Literal ($s)")))))),


Rule ("stringcontents",
   Choice (
      Sequence(Negation(Literal "\\\\"), 
         Sequence(Negation(Literal "\\\""), 
            Sequence(Label ("c",Nonterm "char"), 
               Sequence(Label ("s",Nonterm "stringcontents"), 
                  Result("$c ^ $s"))))),
      Choice (
         Sequence(Label ("b",Literal "\\\\"),
            Sequence(Label ("c",Nonterm "char"), 
               Sequence(Label ("s",Nonterm "stringcontents"), 
                  Result("$b ^ $c ^ $s")))),
         Result(("\"\""))))),


Rule ("choice",
   Choice (
      Sequence(Label ("a",Nonterm "sequence"), 
         Sequence(Literal "/", 
            Sequence(Nonterm "_", 
               Sequence(Label ("b",Nonterm "choice"), 
                  Result("Choice($a,$b)"))))),
      Sequence(Nonterm "sequence", Null))),


Rule ("negation",
   Sequence(Literal "!", 
      Sequence(Nonterm "_", 
         Sequence(Label ("t",Nonterm "term"), 
            Result("Negation ($t)"))))),


Rule ("result_expression",
   Sequence(Literal "->", 
      Sequence(Nonterm "_", 
         Sequence(Label ("result",Nonterm "expr"), 
            Sequence(Nonterm "_", 
               Result("Result ($result)")))))),


Rule ("expr",
   Sequence(Literal "(", 
      Sequence(Nonterm "_", 
         Sequence(Label ("e",Nonterm "exprcontents"), 
            Sequence(Literal ")", 
               Result("($e)")))))),


Rule ("exprcontents",
   Choice (
      Sequence(Label ("c",
         Choice (
            Sequence(Negation(Literal "("), 
               Sequence(Negation(Literal ")"), 
                  Sequence(Nonterm "char", Null))),
            Sequence(Nonterm "expr", Null))), 
         Sequence(Label ("e",Nonterm "exprcontents"), 
            Result("$c ^ $e"))),
      Result(("\"\"")))),

Rule ("parenthesized",
   Sequence(Literal "(", 
      Sequence(Nonterm "_", 
         Sequence(Label ("body",Nonterm "choice"), 
            Sequence(Literal ")", 
               Sequence(Nonterm "_", 
                  Result(("($body)"))))))))]

val defs =
"datatype ast = \n" ^
"   Rule of string * ast\n" ^
" | Choice of ast * ast\n"^ 
" | Sequence of ast * ast\n" ^
" | Literal of string\n" ^
" | Nonterm of string\n" ^
" | Negation of ast\n" ^
" | Result of string\n" ^
" | Label of string * ast\n" ^
" | Star of ast\n" ^
" | Plus of ast\n" ^
" | Null\n\n" ^ 
"type res_rule = ast option\n" ^ 
"type res_sp = string option\n" ^ 
"type res__ = string option\n" ^ 
"type res_sentence = ast list option\n" ^ 
"type res_meta = string option\n" ^ 
"type res_name = string option\n" ^ 
"type res_namechar = string option\n" ^ 
"type res_termfrag = ast option\n" ^ 
"type res_term = ast option\n" ^ 
"type res_nonterminal = ast option\n" ^ 
"type res_labeled = ast option\n" ^ 
"type res_sequence = ast option\n" ^ 
"type res_string = ast option\n" ^ 
"type res_stringcontents = string option\n" ^ 
"type res_choice = ast option\n" ^ 
"type res_negation = ast option\n" ^ 
"type res_result_expression = ast option\n" ^ 
"type res_expr = string option\n" ^ 
"type res_exprcontents = string option\n" ^ 
"type res_parenthesized = ast option\n" ^
"fun notNone (NONE : 'a option) = false | notNone (SOME _) = true\n\n" ^
"fun $ f = valOf f\n\n" ^
"fun kupeg_start buf = valOf (parse_sentence (buf,ref 0))\n" ^
"and parse_char(input, pos) = \n" ^
"  if (!pos >= size input) then NONE else\n" ^
"  (pos := !pos + 1; SOME (String.str (String.sub(input,(!pos - 1)))))\n" ^
"  handle Subscript => NONE\n" ^ 
"and literal(input, pos, str) = \n" ^
"  (if (String.substring(input, !pos, size str) = str) then\n" ^
"  (pos := !pos + size str; SOME  (str))\n" ^
"  else NONE) handle Subscript => NONE\n\n" 

fun main () =
   let
      val fp = TextIO.openOut "parser.sml"
      val _ = TextIO.output (fp,defs ^ 
                     (String.concatWith "\n" (map gen parser)) ^ "\n"
                      )
   in
      TextIO.closeOut fp
   end

val _ = main ()
val _ = Unix.exit 0w0


