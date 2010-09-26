(* Generated from kupeg.kpg *)

type st = { pos : int, va : string option }

fun push (stack, s : st option) = stack := s :: (!stack)

fun pop stack =
   let
      val s = !stack
      val _ = stack := tl s
   in
      hd s
   end


fun pos_ (s : st option ref) =
   let
      val s' = valOf (!s)
   in
      #pos s'
   end

fun va_ (s : st option ref) =
   let
      val s' = valOf (!s)
   in
      valOf (#va s')
   end

fun notnone s = case (!s) of NONE => false
                           | SOME x => true

fun kupeg_make_stack () = ref [] : st option list ref

fun kupeg_join l = String.concatWith "" l


and parse_sp(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, " ")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\n")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := literal(input, pos_ state, "\t")
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
   !state
end
and parse__(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_sp(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  in () end else ignore (pop stack))
in
   !state
end
and parse_rule(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := parse_name(input, pos_ state)
  val n = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "<-")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
  val body = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ".")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join(["and parse_", n, "(input, pos) = \nlet\n",
                   "  val state = ref (SOME {pos = pos, va = NONE})\n",
                   "  val stack = kupeg_make_stack ()\n",
                   body, 
                   "in\n   !state\nend"
                ]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_sentence(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_rule(input, pos_ state)
  val r = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse_sentence(input, pos_ state)
  val g = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([r,"\n",g]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_rule(input, pos_ state)
  val r = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([r,"\n",
                 "and parse_char(input, pos) = \n",
                 "  (if (pos >= size input) then NONE else\n",
                 "  SOME {pos = pos + 1, va = SOME (String.str (String.sub(input,pos)))})\n",
				 "  handle Subscript => NONE\n",
                 "and literal(input, pos, str) = \n",
                 "  (if (String.substring(input, pos, size str) = str) then\n",
                 "    SOME { pos = pos + size str, va = SOME str }\n",
                 "  else NONE) handle Subscript => NONE\n"])
            )} else ()
 in () end) else ()
 in () end) else ()
  in () end else ignore (pop stack))
in
   !state
end
and parse_meta(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "!")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "<-")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "/")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, ".")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "(")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, ")")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, ":")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := literal(input, pos_ state, "->")
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
   !state
end
and parse_name(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_namechar(input, pos_ state)
  val c = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse_name(input, pos_ state)
  val n = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([c,n]))} else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse_namechar(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
in
   !state
end
and parse_namechar(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_meta(input, pos_ state)
  val _ = if notnone state then (pop stack;state := NONE)
  else state := pop stack
  val _ = if (notnone state) then (let
  val _ = push(stack,!state)
  val _ = state := parse_sp(input, pos_ state)
  val _ = if notnone state then (pop stack;state := NONE)
  else state := pop stack
  val _ = if (notnone state) then (let
  val _ = state := parse_char(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_term(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_labeled(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := parse_nonterminal(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := parse_string(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := parse_negation(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse_parenthesized(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
   !state
end
and parse_nonterminal(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := parse_name(input, pos_ state)
  val n = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join(["  val _ = state := parse_", n, "(input, pos_ state)\n"]))} else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_labeled(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := parse_name(input, pos_ state)
  val label = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ":")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_term(input, pos_ state)
  val value = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([value,
			     "  val ", label, " = ",
			     "if (notnone state) then va_ state else \"\"\n"]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_sequence(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_term(input, pos_ state)
  val foo = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse_sequence(input, pos_ state)
  val bar = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([foo, "  val _ = if (notnone state) then (let\n", bar, " in () end) else ()\n"]))} else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := parse_result_expression(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ("")} else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
   !state
end
and parse_string(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
  val s = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join(["  val _ = state := literal(input, pos_ state, \"", s, "\")\n"]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_stringcontents(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\\")
  val _ = if notnone state then (pop stack;state := NONE)
  else state := pop stack
  val _ = if (notnone state) then (let
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if notnone state then (pop stack;state := NONE)
  else state := pop stack
  val _ = if (notnone state) then (let
  val _ = state := parse_char(input, pos_ state)
  val c = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
  val s = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([c, s]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\\")
  val b = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse_char(input, pos_ state)
  val c = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
  val s = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([b, c, s]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ("")} else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
   !state
end
and parse_choice(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_sequence(input, pos_ state)
  val a = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "/")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
  val b = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join(["  val _ = push(stack,!state)\n",
                    a,
                    "  val _ = (if not (notnone state) then let\n",
                    "    val _ = state := pop stack\n",
                    b,
                    "  in () end else ignore (pop stack))\n"]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse_sequence(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
in
   !state
end
and parse_negation(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "!")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_term(input, pos_ state)
  val t = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join(["  val _ = push(stack,!state)\n",
                  t,
                  "  val _ = if notnone state then (",
                  "pop stack;",
                  "state := NONE)\n",
                  "  else state := pop stack\n"]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_result_expression(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "->")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_expr(input, pos_ state)
  val result = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join(["  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ", result, "} else ()\n"]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_expr(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "(")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_exprcontents(input, pos_ state)
  val e = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ")")
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join(["(",e,")"]))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_exprcontents(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = push(stack,!state)
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "(")
  val _ = if notnone state then (pop stack;state := NONE)
  else state := pop stack
  val _ = if (notnone state) then (let
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, ")")
  val _ = if notnone state then (pop stack;state := NONE)
  else state := pop stack
  val _ = if (notnone state) then (let
  val _ = state := parse_char(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse_expr(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
  val c = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := parse_exprcontents(input, pos_ state)
  val e = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_join([c,e]))} else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ("")} else ()
  in () end else ignore (pop stack))
in
   !state
end
and parse_parenthesized(input, pos) = 
let
  val state = ref (SOME {pos = pos, va = NONE})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "(")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
  val body = if (notnone state) then va_ state else ""
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ")")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (body)} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_char(input, pos) = 
  (if (pos >= size input) then NONE else
  SOME {pos = pos + 1, va = SOME (String.str (String.sub(input,pos)))})
  handle Subscript => NONE
and literal(input, pos, str) = 
  (if (String.substring(input, pos, size str) = str) then
    SOME { pos = pos + size str, va = SOME str }
  else NONE) handle Subscript => NONE

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

