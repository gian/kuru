(* Generated from kupeg.kpg *)

type 'a st = { pos : int, va : 'a option }
fun $ f = (valOf f) ()
type kupeg_result_char = string
type kupeg_result_literal = string
type kupeg_result_sp = string
type kupeg_result__ = string
type kupeg_result_rule = string
type kupeg_result_sentence = string
type kupeg_result_meta = string
type kupeg_result_name = string
type kupeg_result_namechar = string
type kupeg_result_termfrag = string
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

datatype kupeg_r = Kupeg_empty
 | Kupeg_r_char of unit -> kupeg_result_char
 | Kupeg_r_literal of unit -> kupeg_result_literal
 | Kupeg_r_sp of unit -> kupeg_result_sp
 | Kupeg_r__ of unit -> kupeg_result__
 | Kupeg_r_rule of unit -> kupeg_result_rule
 | Kupeg_r_sentence of unit -> kupeg_result_sentence
 | Kupeg_r_meta of unit -> kupeg_result_meta
 | Kupeg_r_name of unit -> kupeg_result_name
 | Kupeg_r_namechar of unit -> kupeg_result_namechar
 | Kupeg_r_termfrag of unit -> kupeg_result_termfrag
 | Kupeg_r_term of unit -> kupeg_result_term
 | Kupeg_r_nonterminal of unit -> kupeg_result_nonterminal
 | Kupeg_r_labeled of unit -> kupeg_result_labeled
 | Kupeg_r_sequence of unit -> kupeg_result_sequence
 | Kupeg_r_string of unit -> kupeg_result_string
 | Kupeg_r_stringcontents of unit -> kupeg_result_stringcontents
 | Kupeg_r_choice of unit -> kupeg_result_choice
 | Kupeg_r_negation of unit -> kupeg_result_negation
 | Kupeg_r_result_expression of unit -> kupeg_result_result_expression
 | Kupeg_r_expr of unit -> kupeg_result_expr
 | Kupeg_r_exprcontents of unit -> kupeg_result_exprcontents
 | Kupeg_r_parenthesized of unit -> kupeg_result_parenthesized
fun kupp Kupeg_empty = "empty"
  | kupp (Kupeg_r_char _) = "char"
  | kupp (Kupeg_r_literal _) = "literal"
  | kupp (Kupeg_r_sp _) = "sp"
  | kupp (Kupeg_r__ _) = "_"
  | kupp (Kupeg_r_rule _) = "rule"
  | kupp (Kupeg_r_sentence _) = "sentence"
  | kupp (Kupeg_r_meta _) = "meta"
  | kupp (Kupeg_r_name _) = "name"
  | kupp (Kupeg_r_namechar _) = "namechar"
  | kupp (Kupeg_r_termfrag _) = "termfrag"
  | kupp (Kupeg_r_term _) = "term"
  | kupp (Kupeg_r_nonterminal _) = "nonterminal"
  | kupp (Kupeg_r_labeled _) = "labeled"
  | kupp (Kupeg_r_sequence _) = "sequence"
  | kupp (Kupeg_r_string _) = "string"
  | kupp (Kupeg_r_stringcontents _) = "stringcontents"
  | kupp (Kupeg_r_choice _) = "choice"
  | kupp (Kupeg_r_negation _) = "negation"
  | kupp (Kupeg_r_result_expression _) = "result_expression"
  | kupp (Kupeg_r_expr _) = "expr"
  | kupp (Kupeg_r_exprcontents _) = "exprcontents"
  | kupp (Kupeg_r_parenthesized _) = "parenthesized"

fun unbox_kupeg_char (SOME (Kupeg_r_char f)) = f
  | unbox_kupeg_char (SOME (Kupeg_r_stringcontents f)) = f
  | unbox_kupeg_char (SOME x) = raise Fail ("unbox failed (char)" ^ kupp x)
fun unbox_kupeg_literal (SOME (Kupeg_r_literal f)) = f
  | unbox_kupeg_literal (SOME x) = raise Fail ("unbox failed (literal)" ^ kupp x)
fun unbox_kupeg_sp (SOME (Kupeg_r_sp f)) = f
  | unbox_kupeg_sp (SOME x) = raise Fail ("unbox failed (sp)" ^ kupp x)
fun unbox_kupeg__ (SOME (Kupeg_r__ f)) = f
  | unbox_kupeg__ (SOME x) = raise Fail ("unbox failed (_)" ^ kupp x)
fun unbox_kupeg_rule (SOME (Kupeg_r_rule f)) = f
  | unbox_kupeg_rule (SOME x) = raise Fail ("unbox failed (rule)" ^ kupp x)
fun unbox_kupeg_sentence (SOME (Kupeg_r_sentence f)) = f
  | unbox_kupeg_sentence (SOME x) = raise Fail ("unbox failed (sentence)" ^ kupp x)
fun unbox_kupeg_meta (SOME (Kupeg_r_meta f)) = f
  | unbox_kupeg_meta (SOME (Kupeg_r_literal f)) = f
  | unbox_kupeg_meta (SOME x) = raise Fail ("unbox failed (meta)" ^ kupp x)
fun unbox_kupeg_name (SOME (Kupeg_r_name f)) = f
  | unbox_kupeg_name (SOME x) = raise Fail ("unbox failed (name)" ^ kupp x)
fun unbox_kupeg_namechar (SOME (Kupeg_r_namechar f)) = f
  | unbox_kupeg_namechar (SOME (Kupeg_r_name f)) = f
  | unbox_kupeg_namechar (SOME x) = raise Fail ("unbox failed (namechar)" ^ kupp x)
fun unbox_kupeg_termfrag (SOME (Kupeg_r_termfrag f)) = f
  | unbox_kupeg_termfrag (SOME x) = raise Fail ("unbox failed (termfrag)" ^ kupp x)
fun unbox_kupeg_term (SOME (Kupeg_r_term f)) = f
  | unbox_kupeg_term (SOME x) = raise Fail ("unbox failed (term)" ^ kupp x)
fun unbox_kupeg_nonterminal (SOME (Kupeg_r_nonterminal f)) = f
  | unbox_kupeg_nonterminal (SOME x) = raise Fail ("unbox failed (nonterminal)" ^ kupp x)
fun unbox_kupeg_labeled (SOME (Kupeg_r_labeled f)) = f
  | unbox_kupeg_labeled (SOME x) = raise Fail ("unbox failed (labeled)" ^ kupp x)
fun unbox_kupeg_sequence (SOME (Kupeg_r_sequence f)) = f
  | unbox_kupeg_sequence (SOME x) = raise Fail ("unbox failed (sequence)" ^ kupp x)
fun unbox_kupeg_string (SOME (Kupeg_r_string f)) = f
  | unbox_kupeg_string (SOME x) = raise Fail ("unbox failed (string)" ^ kupp x)
fun unbox_kupeg_stringcontents (SOME (Kupeg_r_stringcontents f)) = f
  | unbox_kupeg_stringcontents (SOME (Kupeg_r_literal f)) = f
  | unbox_kupeg_stringcontents (SOME x) = raise Fail ("unbox failed (stringcontents)" ^ kupp x)
fun unbox_kupeg_choice (SOME (Kupeg_r_choice f)) = f
  | unbox_kupeg_choice (SOME x) = raise Fail ("unbox failed (choice)" ^ kupp x)
fun unbox_kupeg_negation (SOME (Kupeg_r_negation f)) = f
  | unbox_kupeg_negation (SOME x) = raise Fail ("unbox failed (negation)" ^ kupp x)
fun unbox_kupeg_result_expression (SOME (Kupeg_r_result_expression f)) = f
  | unbox_kupeg_result_expression (SOME x) = raise Fail ("unbox failed (result_expression)" ^ kupp x)
fun unbox_kupeg_expr (SOME (Kupeg_r_expr f)) = f
  | unbox_kupeg_expr (SOME (Kupeg_r_char f)) = f
  | unbox_kupeg_expr (SOME x) = raise Fail ("unbox failed (expr)" ^ kupp x)
fun unbox_kupeg_exprcontents (SOME (Kupeg_r_exprcontents f)) = f
  | unbox_kupeg_exprcontents (SOME x) = raise Fail ("unbox failed (exprcontents)" ^ kupp x)
fun unbox_kupeg_parenthesized (SOME (Kupeg_r_parenthesized f)) = f
  | unbox_kupeg_parenthesized (SOME x) = raise Fail ("unbox failed (parenthesized)" ^ kupp x)

fun box_kupeg_char f = Kupeg_r_char (fn () => f)
fun box_kupeg_literal f = Kupeg_r_literal (fn () => f)
fun box_kupeg_sp f = Kupeg_r_sp (fn () => f)
fun box_kupeg__ f = Kupeg_r__ (fn () => f)
fun box_kupeg_rule f = Kupeg_r_rule (fn () => f)
fun box_kupeg_sentence f = Kupeg_r_sentence (fn () => f)
fun box_kupeg_meta f = Kupeg_r_meta (fn () => f)
fun box_kupeg_name f = Kupeg_r_name (fn () => f)
fun box_kupeg_namechar f = Kupeg_r_namechar (fn () => f)
fun box_kupeg_termfrag f = Kupeg_r_termfrag (fn () => f)
fun box_kupeg_term f = Kupeg_r_term (fn () => f)
fun box_kupeg_nonterminal f = Kupeg_r_nonterminal (fn () => f)
fun box_kupeg_labeled f = Kupeg_r_labeled (fn () => f)
fun box_kupeg_sequence f = Kupeg_r_sequence (fn () => f)
fun box_kupeg_string f = Kupeg_r_string (fn () => f)
fun box_kupeg_stringcontents f = Kupeg_r_stringcontents (fn () => f)
fun box_kupeg_choice f = Kupeg_r_choice (fn () => f)
fun box_kupeg_negation f = Kupeg_r_negation (fn () => f)
fun box_kupeg_result_expression f = Kupeg_r_result_expression (fn () => f)
fun box_kupeg_expr f = Kupeg_r_expr (fn () => f)
fun box_kupeg_exprcontents f = Kupeg_r_exprcontents (fn () => f)
fun box_kupeg_parenthesized f = Kupeg_r_parenthesized (fn () => f)

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



val kplineNum = ref 0 
val kpfileName = ref "__dummy__"

val ntName = ref ""

fun kupeg_coerce_namechar (Kupeg_r_char s) = Kupeg_r_namechar s
  | kupeg_coerce_namechar _ = raise Fail "type violation!"

fun kupeg_coerce_sp (Kupeg_r_sp s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r__ s) = Kupeg_r_sp s
  | kupeg_coerce_sp (Kupeg_r_literal s) = Kupeg_r_sp s
  | kupeg_coerce_sp s = raise Fail ("type violate sp /= " ^ kupp s)
(*  | kupeg_coerce_sp (Kupeg_r_rule s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r_sentence s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r_meta s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r_choice s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r_ s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r__ s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r__ s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r__ s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r__ s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r__ s) = Kupeg_r_sp s 
  | kupeg_coerce_sp (Kupeg_r__ s) = Kupeg_r_sp s *)
fun kupeg_coerce__ _ = raise Fail "type violation _!"
fun kupeg_coerce_rule _ = raise Fail "type violation rule!"
fun kupeg_coerce_sentence _ = raise Fail "type violation sentence!"
fun kupeg_coerce_meta (Kupeg_r_literal s) = Kupeg_r_meta s
  | kupeg_coerce_meta _ = raise Fail "type violation meta!"
fun kupeg_coerce_choice (Kupeg_r_sequence s) = Kupeg_r_choice s 
  | kupeg_coerce_choice x = raise Fail ("type violation choice!" ^ kupp x)

fun kupeg_coerce_name (Kupeg_r_namechar s) = Kupeg_r_name s 

(*
fun kupeg_coerce_name s = raise Fail ("type violate name /= " ^ kupp s)
*)

fun kupeg_coerce_termfrag (Kupeg_r_string s) = Kupeg_r_termfrag s
  | kupeg_coerce_termfrag (Kupeg_r_nonterminal s) = Kupeg_r_termfrag s
  | kupeg_coerce_termfrag (Kupeg_r_labeled s) = Kupeg_r_termfrag s
  | kupeg_coerce_termfrag (Kupeg_r_sequence s) = Kupeg_r_termfrag s
  | kupeg_coerce_termfrag (Kupeg_r_choice s) = Kupeg_r_termfrag s
  | kupeg_coerce_termfrag (Kupeg_r_negation s) = Kupeg_r_termfrag s
  | kupeg_coerce_termfrag (Kupeg_r_parenthesized s) = Kupeg_r_termfrag s
  | kupeg_coerce_termfrag x = raise Fail ("type violation termfrag!" ^ kupp x)
fun kupeg_coerce_term _ = raise Fail "type violation term!"
fun kupeg_coerce_nonterminal _ = raise Fail "type violation nonterminal!"
fun kupeg_coerce_labeled _ = raise Fail "type violation labeled!"
fun kupeg_coerce_sequence (Kupeg_r_result_expression s) = Kupeg_r_sequence s
  | kupeg_coerce_sequence _ = raise Fail "type violation sequence!"
fun kupeg_coerce_string _ = raise Fail "type violation string!"
fun kupeg_coerce_stringcontents (Kupeg_r_result_expression s) = Kupeg_r_stringcontents s
  | kupeg_coerce_stringcontents _ = raise Fail "type violation stringcontents!"
fun kupeg_coerce_negation _ = raise Fail "type violation negation!"
fun kupeg_coerce_result_expression _ = raise Fail "type violation result_expression!"
fun kupeg_coerce_expr _ = raise Fail "type violation expr!"
fun kupeg_coerce_exprcontents _ = raise Fail "type violation exprcontents!"
fun kupeg_coerce_parenthesized _ = raise Fail "type violation parenthesized!"


fun kupeg_start s = 
	let
		val p = parse_sentence(s,0) handle Option => raise Fail "p failed"
		val p' = valOf p handle Option => raise Fail "p' failed"
		val p'' = #va p'
	in
		(unbox_kupeg_sentence (p'')) ()
	end handle Option => raise Fail "Outer Parse failed."
(*#line 61.1 "kupeg.kpg" *)and parse_sp(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_sp
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, " ")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\n")
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box (((kplineNum := 1 + !kplineNum); "\n")))} else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := literal(input, pos_ state, "\t")
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_sp _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_sp va)})
(!state)
end
(*#line 62.1 "kupeg.kpg" *)and parse__(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg__
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
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r__ _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce__ va)})
(!state)
end
(*#line 67.1 "kupeg.kpg" *)and parse_rule(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_rule
  val _ = state := parse_name(input, pos_ state)
(* label: name *)
  val n = (if (notnone state) then SOME (unbox_kupeg_name (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "<-")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
(* label: choice *)
  val body = (if (notnone state) then SOME (unbox_kupeg_choice (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ".")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([
                   "(*#line ", Int.toString (!kplineNum), ".1 \"",
                   !kpfileName, "\" *)",
                   "and parse_", $n, "(input, pos) = \nlet\n",
                   "  val state = ref (SOME {pos = pos, ",
                   " va = NONE : kupeg_r option})\n",
                   "  val stack = kupeg_make_stack ()\n",
                   "  val kupeg_box = box_kupeg_", $n, "\n",
                   $body, 
                   "in\n",
                   "(fn NONE => NONE ",
                   "| (s as (SOME {pos=_,va=SOME (Kupeg_r_",$n," _)})) => s ",
                   "| (s as (SOME {pos=_,va=NONE})) => s ",
                   "| (SOME {pos=pos,va=SOME va}) => SOME {",
                   "pos=pos,va=SOME (kupeg_coerce_",$n," va)",
                   "})\n",
                   "(!state)\nend"
                ]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_rule _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_rule va)})
(!state)
end
(*#line 69.1 "kupeg.kpg" *)and parse_sentence(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_sentence
  val _ = push(stack,!state)
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_rule(input, pos_ state)
(* label: rule *)
  val r = (if (notnone state) then SOME (unbox_kupeg_rule (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse_sentence(input, pos_ state)
(* label: sentence *)
  val g = (if (notnone state) then SOME (unbox_kupeg_sentence (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$r,"\n",$g]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_rule(input, pos_ state)
(* label: rule *)
  val r = (if (notnone state) then SOME (unbox_kupeg_rule (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$r,"\n",
                 "and parse_char(input, pos) = \n",
                 "  (if (pos >= size input) then NONE else\n",
                 "  SOME {pos = pos + 1, va = SOME (",
                 "Kupeg_r_char (fn _ => String.str ",
                 "(String.sub(input,pos))))})\n",
				 "  handle Subscript => NONE\n",
                 "and literal(input, pos, str) = \n",
                 "  (if (String.substring(input, pos, size str) = str)",
                 " then\n",
                 "    SOME { pos = pos + size str, va = SOME ",
                 " (Kupeg_r_literal (fn _ => str)) }\n",
                 "  else NONE) handle Subscript => NONE\n"])
            )))} else ()
 in () end) else ()
 in () end) else ()
  in () end else ignore (pop stack))
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_sentence _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_sentence va)})
(!state)
end
(*#line 71.1 "kupeg.kpg" *)and parse_meta(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_meta
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
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "->")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "*")
  val _ = if (notnone state) then (let
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := literal(input, pos_ state, "+")
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
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_meta _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_meta va)})
(!state)
end
(*#line 72.1 "kupeg.kpg" *)and parse_name(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_name
  val _ = push(stack,!state)
  val _ = state := parse_namechar(input, pos_ state)
(* label: namechar *)
  val c = (if (notnone state) then SOME (unbox_kupeg_namechar (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse_name(input, pos_ state)
(* label: name *)
  val n = (if (notnone state) then SOME (unbox_kupeg_name (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$c,$n]))))} else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = state := parse_namechar(input, pos_ state)
  val _ = if (notnone state) then (let
 in () end) else ()
  in () end else ignore (pop stack))
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_name _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_name va)})
(!state)
end
(*#line 73.1 "kupeg.kpg" *)and parse_namechar(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_namechar
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
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_namechar _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_namechar va)})
(!state)
end
(*#line 74.1 "kupeg.kpg" *)and parse_termfrag(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_termfrag
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
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_termfrag _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_termfrag va)})
(!state)
end
(*#line 83.1 "kupeg.kpg" *)and parse_term(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_term
  val _ = state := parse_termfrag(input, pos_ state)
(* label: termfrag *)
  val t = (if (notnone state) then SOME (unbox_kupeg_termfrag (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "*")
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join["(* repetition_start *)", 
                         $t, 
                         "(* repetition_end *)"])))} else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "+")
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box (("")))} else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box (($t)))} else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
  val _ = if (notnone state) then (let
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_term _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_term va)})
(!state)
end
(*#line 88.1 "kupeg.kpg" *)and parse_nonterminal(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_nonterminal
  val _ = state := parse_name(input, pos_ state)
(* label: name *)
  val n = (if (notnone state) then SOME (unbox_kupeg_name (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((ntName := $n; kupeg_join(["  val _ = state := parse_", 
                    $n, "(input, pos_ state)\n"]))))} else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_nonterminal _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_nonterminal va)})
(!state)
end
(*#line 93.1 "kupeg.kpg" *)and parse_labeled(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_labeled
  val _ = state := parse_name(input, pos_ state)
(* label: name *)
  val label = (if (notnone state) then SOME (unbox_kupeg_name (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ":")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_term(input, pos_ state)
(* label: term *)
  val value = (if (notnone state) then SOME (unbox_kupeg_term (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$value,
                 "(* label: ", !ntName, " *)\n",
			     "  val ", $label, " = ",
                 " (",
			     "if (notnone state) then SOME (",
				  "unbox_kupeg_", !ntName,
                 " (va_ state))",
                 " else NONE)\n"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_labeled _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_labeled va)})
(!state)
end
(*#line 98.1 "kupeg.kpg" *)and parse_sequence(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_sequence
  val _ = push(stack,!state)
  val _ = state := parse_term(input, pos_ state)
(* label: term *)
  val foo = (if (notnone state) then SOME (unbox_kupeg_term (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse_sequence(input, pos_ state)
(* label: sequence *)
  val bar = (if (notnone state) then SOME (unbox_kupeg_sequence (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$foo, "  val _ = if (notnone state) ",
                                "then ",
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
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box (("")))} else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_sequence _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_sequence va)})
(!state)
end
(*#line 103.1 "kupeg.kpg" *)and parse_string(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_string
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
(* label: stringcontents *)
  val s = (if (notnone state) then SOME (unbox_kupeg_stringcontents (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "\"")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join(["  val _ = state := ",
                          "literal(input, pos_ state, \"", $s, "\")\n"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_string _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_string va)})
(!state)
end
(*#line 108.1 "kupeg.kpg" *)and parse_stringcontents(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_stringcontents
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
(* label: char *)
  val c = (if (notnone state) then SOME (unbox_kupeg_char (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
(* label: stringcontents *)
  val s = (if (notnone state) then SOME (unbox_kupeg_stringcontents (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$c, $s]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = push(stack,!state)
  val _ = state := literal(input, pos_ state, "\\")
(* label: stringcontents *)
  val b = (if (notnone state) then SOME (unbox_kupeg_stringcontents (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse_char(input, pos_ state)
(* label: char *)
  val c = (if (notnone state) then SOME (unbox_kupeg_char (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse_stringcontents(input, pos_ state)
(* label: stringcontents *)
  val s = (if (notnone state) then SOME (unbox_kupeg_stringcontents (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$b, $c, $s]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box (("")))} else ()
  in () end else ignore (pop stack))
  in () end else ignore (pop stack))
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_stringcontents _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_stringcontents va)})
(!state)
end
(*#line 111.1 "kupeg.kpg" *)and parse_choice(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_choice
  val _ = push(stack,!state)
  val _ = state := parse_sequence(input, pos_ state)
(* label: sequence *)
  val a = (if (notnone state) then SOME (unbox_kupeg_sequence (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, "/")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
(* label: choice *)
  val b = (if (notnone state) then SOME (unbox_kupeg_choice (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join(["  val _ = push(stack,!state)\n",
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
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_choice _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_choice va)})
(!state)
end
(*#line 116.1 "kupeg.kpg" *)and parse_negation(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_negation
  val _ = state := literal(input, pos_ state, "!")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_term(input, pos_ state)
(* label: term *)
  val t = (if (notnone state) then SOME (unbox_kupeg_term (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join(["  val _ = push(stack,!state)\n",
                  $t,
                  "  val _ = if notnone state then (",
                  "pop stack;",
                  "state := NONE)\n",
                  "  else state := pop stack\n"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_negation _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_negation va)})
(!state)
end
(*#line 121.1 "kupeg.kpg" *)and parse_result_expression(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_result_expression
  val _ = state := literal(input, pos_ state, "->")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_expr(input, pos_ state)
(* label: expr *)
  val result = (if (notnone state) then SOME (unbox_kupeg_expr (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([
                                      "  val _ = if (notnone state) then",
                                      " state := SOME {pos = pos_ state,",
                                      " va = SOME (kupeg_box (", $result, "))}",
                                      " else ()\n"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_result_expression _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_result_expression va)})
(!state)
end
(*#line 122.1 "kupeg.kpg" *)and parse_expr(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_expr
  val _ = state := literal(input, pos_ state, "(")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_exprcontents(input, pos_ state)
(* label: exprcontents *)
  val e = (if (notnone state) then SOME (unbox_kupeg_exprcontents (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ")")
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join(["(",$e,")"]))))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_expr _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_expr va)})
(!state)
end
(*#line 125.1 "kupeg.kpg" *)and parse_exprcontents(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_exprcontents
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
(* label: expr *)
  val c = (if (notnone state) then SOME (unbox_kupeg_expr (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := parse_exprcontents(input, pos_ state)
(* label: exprcontents *)
  val e = (if (notnone state) then SOME (unbox_kupeg_exprcontents (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box ((kupeg_join([$c,$e]))))} else ()
 in () end) else ()
 in () end) else ()
  val _ = (if not (notnone state) then let
    val _ = state := pop stack
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box (("")))} else ()
  in () end else ignore (pop stack))
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_exprcontents _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_exprcontents va)})
(!state)
end
(*#line 129.1 "kupeg.kpg" *)and parse_parenthesized(input, pos) = 
let
  val state = ref (SOME {pos = pos,  va = NONE : kupeg_r option})
  val stack = kupeg_make_stack ()
  val kupeg_box = box_kupeg_parenthesized
  val _ = state := literal(input, pos_ state, "(")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = state := parse_choice(input, pos_ state)
(* label: choice *)
  val body = (if (notnone state) then SOME (unbox_kupeg_choice (va_ state)) else NONE)
  val _ = if (notnone state) then (let
  val _ = state := literal(input, pos_ state, ")")
  val _ = if (notnone state) then (let
  val _ = state := parse__(input, pos_ state)
  val _ = if (notnone state) then (let
  val _ = if (notnone state) then state := SOME {pos = pos_ state, va = SOME (kupeg_box (($body)))} else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
 in () end) else ()
in
(fn NONE => NONE | (s as (SOME {pos=_,va=SOME (Kupeg_r_parenthesized _)})) => s | (s as (SOME {pos=_,va=NONE})) => s | (SOME {pos=pos,va=SOME va}) => SOME {pos=pos,va=SOME (kupeg_coerce_parenthesized va)})
(!state)
end
and parse_char(input, pos) = 
  (if (pos >= size input) then NONE else
  SOME {pos = pos + 1, va = SOME (Kupeg_r_char (fn _ => String.str (String.sub(input,pos))))})
  handle Subscript => NONE
and literal(input, pos, str) = 
  (if (String.substring(input, pos, size str) = str) then
    SOME { pos = pos + size str, va = SOME  (Kupeg_r_literal (fn _ => str)) }
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
      val _ = kpfileName := filename
 
      val startFn = ref ""
      val resultTy = ref ""
      val emptyVal = ref ""
      val nontermTypes = ref [("char","string"),
                              ("literal","string")] 
                                : (string * string) list ref

      fun startSymbol s = startFn := 
        ("fun kupeg_start s = (unbox_kupeg_" ^ s ^ " " ^ 
            "(#va (valOf (parse_" ^ s ^ "(s,0))))) () " ^
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

           fun g [] = ""
             | g ((n,d)::t) = 
                " | Kupeg_r_" ^ n ^ 
                    " of unit -> kupeg_result_" ^ n ^ "\n" ^
                        g t

           fun g' [] = ""
             | g' ((n,d)::t) =
                "  | kupp (Kupeg_r_" ^ n ^ " _) = \"" ^ n ^ "\"\n" ^
                    g' t

           fun h [] = ""
             | h ((n,d)::t) = 
                "fun unbox_kupeg_" ^ n ^ " (Kupeg_r_" ^ n ^ 
                    " f) = f\n  | unbox_kupeg_" ^ n ^
                    " x = raise Fail (\"unbox failed ("^ n ^")\" ^ kupp x)\n" ^
                        h t
           fun i [] = ""
             | i ((n,d)::t) = 
                "fun box_kupeg_" ^ n ^   
                    " f = Kupeg_r_" ^ n ^ " (fn () => f)\n" ^
                        i t
           
        in
            f (!nontermTypes) ^ "\ndatatype kupeg_r = " ^
                "Kupeg_empty\n" ^ g (!nontermTypes) ^ 
                "fun kupp Kupeg_empty = \"empty\"\n" ^ g' (!nontermTypes) ^
                "\n" ^ h (!nontermTypes) ^ "\n" ^ i (!nontermTypes) ^
                "\n"
        end

      fun readLines fp = 
         let
            val l = TextIO.inputLine fp
            val _ = kplineNum := 1 + !kplineNum
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
      val kpl = !kplineNum
      val buf = readLines f
      val _ = kplineNum := kpl

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
      val _ = TextIO.output (fo, "fun $ f = (valOf f) ()\n") 
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

