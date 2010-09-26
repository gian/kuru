(* Generated from kupeg.kpg *)

type 'a st = { pos : int, va : 'a option }
val $ = valOf
type kupeg_result_sp = string
type kupeg_result__ = string
type kupeg_result_rule = string
type kupeg_result_sentence = string
type kupeg_result_meta = string
type kupeg_result_name = string
type kupeg_result_namechar = string
type kupeg_result_term = string
type kupeg_result_nonterminal = string
type kupeg_result_labeled = string
type kupeg_result_sequence = string
type kupeg_result_string = string
type kupeg_result_stringcontents = string
type kupeg_result_choice = string
type kupeg_result_negation = string
type kupeg_result_result_expression = string
type kupeg_result_expr = string
type kupeg_result_exprcontents = string
type kupeg_result_parenthesized = string
fun push (stack, s : 'a st option) = stack := s :: (!stack)

fun pop stack =
let
   val s = !stack
   val _ = stack := tl s
in hd s end

fun pos_ (s : 'a st option ref) =
let
   val s' = valOf (!s)
in #pos s' end

fun va_ (s : 'a st option ref) =
let
   val s' = valOf (!s)
in #va s' end

fun notnone s = case (!s) of NONE => false
                           | SOME x => true

fun kupeg_make_stack () = ref [] : 'a st option list ref
(* Based on peg-bootstrap by Kragen Javier Sitaker *)
(* http://github.com/kragen/peg-bootstrap *)
(* Ported to Kuru by Gian Perrone *)
(* http://www.kuru-lang.org *)

fun kupeg_join l = String.concatWith "" l




fun kupeg_start s = (valOf (#va (valOf (parse_sentence(s,0))))) handle Option => raise Fail "Parse failed."
and parse_sp(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_sp option})
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result__ option})
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_rule option})
  val stack = kupeg_make_stack ()
  val _ = state := parse_name(input, pos_ state)
  val n = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "<-")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
  val body = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ".")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join(["and parse_", $n, "(input, pos) = \nlet\n",
                   "  val state = ref (SOME {pos = pos, ",
                   " va = NONE : kupeg_result_", $n," option})\n",
                   "  val stack = kupeg_make_stack ()\n",
                   $body, 
                   "in\n   !state\nend"
                ]))))} else ()
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_sentence option})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_rule(input, pos_ state)
  val r = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse_sentence(input, pos_ state)
  val g = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$r,"\n",$g]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_rule(input, pos_ state)
  val r = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$r,"\n",
                 "and parse_char(input, pos) = \n",
                 "  (if (pos >= size input) then NONE else\n",
                 "  SOME {pos = pos + 1, va = SOME (String.str (String.sub(input,pos)))})\n",
				 "  handle Subscript => NONE\n",
                 "and literal(input, pos, str) = \n",
                 "  (if (String.substring(input, pos, size str) = str) then\n",
                 "    SOME { pos = pos + size str, va = SOME str }\n",
                 "  else NONE) handle Subscript => NONE\n"])
            )))} else ()
 in () end) else ()
 in () end) else ()
  in () end else ignore (pop stack))
in
   !state
end
and parse_meta(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_meta option})
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_name option})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_namechar(input, pos_ state)
  val c = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse_name(input, pos_ state)
  val n = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$c,$n]))))} else ()
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_namechar option})
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_term option})
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_nonterminal option})
  val stack = kupeg_make_stack ()
  val _ = state := parse_name(input, pos_ state)
  val n = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join(["  val _ = state := parse_", $n, 
                                "(input, pos_ state)\n"]))))} else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_labeled(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_labeled option})
  val stack = kupeg_make_stack ()
  val _ = state := parse_name(input, pos_ state)
  val label = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ":")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_term(input, pos_ state)
  val value = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$value,
			     "  val ", $label, " = ",
			     "if (notnone state) then ",
                 " (va_ state)",
                 " else NONE\n"]))))} else ()
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_sequence option})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_term(input, pos_ state)
  val foo = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse_sequence(input, pos_ state)
  val bar = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$foo, "  val _ = if (notnone state) then ",
                                "(let\n", $bar, " in () end) else ()\n"]))))} else ()
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
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ((("")))} else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
   !state
end
and parse_string(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_string option})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
  val s = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join(["  val _ = state := ",
                          "literal(input, pos_ state, \"", $s, "\")\n"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_stringcontents(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_stringcontents option})
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
  val c = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
  val s = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$c, $s]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\\")
  val b = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse_char(input, pos_ state)
  val c = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
  val s = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$b, $c, $s]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ((("")))} else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
   !state
end
and parse_choice(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_choice option})
  val stack = kupeg_make_stack ()
  val _ = push(stack,!state)
  val _ = state := parse_sequence(input, pos_ state)
  val a = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "/")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
  val b = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join(["  val _ = push(stack,!state)\n",
                    $a,
                    "  val _ = (if not (notnone state) then let\n",
                    "    val _ = state := pop stack\n",
                    $b,
                    "  in () end else ignore (pop stack))\n"]))))} else ()
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
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_negation option})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "!")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_term(input, pos_ state)
  val t = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join(["  val _ = push(stack,!state)\n",
                  $t,
                  "  val _ = if notnone state then (",
                  "pop stack;",
                  "state := NONE)\n",
                  "  else state := pop stack\n"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_result_expression(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_result_expression option})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "->")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_expr(input, pos_ state)
  val result = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([
                                      "  val _ = if (notnone state) then ",
                                      "state := SOME {pos = pos_ state, ",
                                      "va = SOME ((", $result, "))}",
                                      " else ()\n"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_expr(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_expr option})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "(")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_exprcontents(input, pos_ state)
  val e = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ")")
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join(["(",$e,")"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
   !state
end
and parse_exprcontents(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_exprcontents option})
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
  val c = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := parse_exprcontents(input, pos_ state)
  val e = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (((kupeg_join([$c,$e]))))} else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ((("")))} else ()
  in () end else ignore (pop stack))
in
   !state
end
and parse_parenthesized(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_result_parenthesized option})
  val stack = kupeg_make_stack ()
  val _ = state := literal(input, pos_ state, "(")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
  val body = if (notnone state) then  (va_ state) else NONE
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ")")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME ((($body)))} else ()
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

