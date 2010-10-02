(*******************************************************************************
*  The Kuru Programming Language Compiler Toolset (http://www.kuru-lang.org)
*  Copyright (C) 2010  Gian Perrone
*
*  This program is free software: you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*******************************************************************************
*  This file:
*    A bootstrap Kupeg parser for Kupeg grammars in SML.
******************************************************************************)
(* Generated from kupeg.kpg *)

structure KupegParser =
struct

(* Based on peg-bootstrap by Kragen Javier Sitaker *)
(* http://github.com/kragen/peg-bootstrap *)
(* Ported to Kuru by Gian Perrone *)
(* http://www.kuru-lang.org *)

fun kupeg_join l = String.concatWith "" l


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


val kplineNum = ref 0 
val kpfileName = ref "__dummy__"


type res_char = string option
type res_literal = string option
type res_sp = string option
type res__ = string option
type res_rule = ast option
type res_sentence = ast list option
type res_meta = string option
type res_name = string option
type res_namechar = string option
type res_termfrag = ast option
type res_term = ast option
type res_nonterminal = ast option
type res_labeled = ast option
type res_sequence = ast option
type res_string = ast option
type res_stringcontents = string option
type res_choice = ast option
type res_negation = ast option
type res_result_expression = ast option
type res_expr = string option
type res_exprcontents = string option
type res_parenthesized = ast option

fun $ f = valOf f

val debugVerbose = ref false
fun debug_print s = if (!debugVerbose) then print s else ()
fun notNone (NONE : 'a option) = false | notNone (SOME _) = true
fun kupeg_start buf = valOf (parse_sentence (buf,ref 0))
and parse_char(input, pos) = 
  if (!pos >= size input) then NONE else
  (pos := !pos + 1; SOME (String.str (String.sub(input,(!pos - 1)))))
  handle Subscript => NONE
and literal(input, pos, str) = 
  (if (String.substring(input, !pos, size str) = str) then
  (pos := !pos + size str; SOME  (str))
  else NONE) handle Subscript => NONE

and parse_sp(input,pos) : res_sp =
   let
      val _ = debug_print "parse_sp\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 = literal(input,pos," ")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"\n")
   in
      if notNone opt1 then opt1 else
      (pos := prestate; literal(input,pos,"\t"))
   end
)
   end

   end


and parse__(input,pos) : res__ =
   let
      val _ = debug_print "parse__\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse_sp(input,pos)
   in
      if notNone s1' then parse__(input,pos)   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME "")
   end

   end


and parse_rule(input,pos) : res_rule =
   let
      val _ = debug_print "parse_rule\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val n = parse_name(input,pos)
   in
      if notNone n then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"<-")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val body = parse_choice(input,pos)
   in
      if notNone body then (   let
      val prestate = !pos
      val s1' = literal(input,pos,".")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME (((Rule($n,$body))))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   end


and parse_sentence(input,pos) : res_sentence =
   let
      val _ = debug_print "parse_sentence\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val r = parse_rule(input,pos)
   in
      if notNone r then (   let
      val prestate = !pos
      val g = parse_sentence(input,pos)
   in
      if notNone g then (SOME (($r :: $g))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val r = parse_rule(input,pos)
   in
      if notNone r then (SOME (($r :: []))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end

   end


and parse_meta(input,pos) : res_meta =
   let
      val _ = debug_print "parse_meta\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 = literal(input,pos,"!")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"\"")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"<-")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"/")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,".")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"(")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,")")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,":")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"->")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"*")
   in
      if notNone opt1 then opt1 else
      (pos := prestate; literal(input,pos,"+"))
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end

   end


and parse_name(input,pos) : res_name =
   let
      val _ = debug_print "parse_name\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val c = parse_namechar(input,pos)
   in
      if notNone c then (   let
      val prestate = !pos
      val n = parse_name(input,pos)
   in
      if notNone n then (SOME (($c ^ $n))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; parse_namechar(input,pos))
   end

   end


and parse_namechar(input,pos) : res_namechar =
   let
      val _ = debug_print "parse_namechar\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = parse_meta(input,pos)
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = parse_sp(input,pos)
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then parse_char(input,pos)   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   end


and parse_termfrag(input,pos) : res_termfrag =
   let
      val _ = debug_print "parse_termfrag\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 = parse_labeled(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = parse_nonterminal(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = parse_string(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = parse_negation(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate; parse_parenthesized(input,pos))
   end
)
   end
)
   end
)
   end

   end


and parse_term(input,pos) : res_term =
   let
      val _ = debug_print "parse_term\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val t = parse_termfrag(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = literal(input,pos,"*")
   in
      if notNone s1' then SOME ((Star($t)))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val t = parse_termfrag(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = literal(input,pos,"+")
   in
      if notNone s1' then SOME ((Plus($t)))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val t = parse_termfrag(input,pos)
   in
      if notNone t then (SOME (($t))) else (pos := prestate;NONE)
   end
)
   end
)
   end

   end


and parse_nonterminal(input,pos) : res_nonterminal =
   let
      val _ = debug_print "parse_nonterminal\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val n = parse_name(input,pos)
   in
      if notNone n then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME ((Nonterm ($n)))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   end


and parse_labeled(input,pos) : res_labeled =
   let
      val _ = debug_print "parse_labeled\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val label = parse_name(input,pos)
   in
      if notNone label then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,":")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val value = parse_term(input,pos)
   in
      if notNone value then (SOME ((Label($label,$value)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   end


and parse_sequence(input,pos) : res_sequence =
   let
      val _ = debug_print "parse_sequence\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val foo = parse_term(input,pos)
   in
      if notNone foo then (   let
      val prestate = !pos
      val bar = parse_sequence(input,pos)
   in
      if notNone bar then (SOME ((Sequence($foo, $bar)))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = parse_result_expression(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME ((Null)))
   end
)
   end

   end


and parse_string(input,pos) : res_string =
   let
      val _ = debug_print "parse_string\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val s1' = literal(input,pos,"\"")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s = parse_stringcontents(input,pos)
   in
      if notNone s then (   let
      val prestate = !pos
      val s1' = literal(input,pos,"\"")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME ((Literal ($s)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   end


and parse_stringcontents(input,pos) : res_stringcontents =
   let
      val _ = debug_print "parse_stringcontents\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = literal(input,pos,"\\")
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = literal(input,pos,"\"")
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_char(input,pos)
   in
      if notNone c then (   let
      val prestate = !pos
      val s = parse_stringcontents(input,pos)
   in
      if notNone s then (SOME (($c ^ $s))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val b = literal(input,pos,"\\")
   in
      if notNone b then (   let
      val prestate = !pos
      val c = parse_char(input,pos)
   in
      if notNone c then (   let
      val prestate = !pos
      val s = parse_stringcontents(input,pos)
   in
      if notNone s then (SOME (($b ^ $c ^ $s))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME (("")))
   end
)
   end

   end


and parse_choice(input,pos) : res_choice =
   let
      val _ = debug_print "parse_choice\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val a = parse_sequence(input,pos)
   in
      if notNone a then (   let
      val prestate = !pos
      val s1' = literal(input,pos,"/")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val b = parse_choice(input,pos)
   in
      if notNone b then (SOME ((Choice($a,$b)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; parse_sequence(input,pos))
   end

   end


and parse_negation(input,pos) : res_negation =
   let
      val _ = debug_print "parse_negation\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val s1' = literal(input,pos,"!")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_term(input,pos)
   in
      if notNone t then (SOME ((Negation ($t)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   end


and parse_result_expression(input,pos) : res_result_expression =
   let
      val _ = debug_print "parse_result_expression\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val s1' = literal(input,pos,"->")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val result = parse_expr(input,pos)
   in
      if notNone result then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME ((Result ($result)))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   end


and parse_expr(input,pos) : res_expr =
   let
      val _ = debug_print "parse_expr\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exprcontents(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME (("(" ^ $e ^ ")"))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   end


and parse_exprcontents(input,pos) : res_exprcontents =
   let
      val _ = debug_print "parse_exprcontents\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val c =    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = literal(input,pos,"(")
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = literal(input,pos,")")
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then parse_char(input,pos)   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; parse_expr(input,pos))
   end

   in
      if notNone c then (   let
      val prestate = !pos
      val e = parse_exprcontents(input,pos)
   in
      if notNone e then (SOME (($c ^ $e))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME (("")))
   end

   end


and parse_parenthesized(input,pos) : res_parenthesized =
   let
      val _ = debug_print "parse_parenthesized\n"
      val stack = ref [] : int list ref
   in
         let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val body = parse_choice(input,pos)
   in
      if notNone body then (   let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME ((($body)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   end
end

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
     | gen (K.Star t) = (print "Star!\n"; 
     "   let\n" ^
     "      val fx = fn () => let\n" ^ 
     "         val prestate = !pos\n" ^
     "         val t = (" ^ gen t ^ ")\n" ^
     "         in if notNone t then (pos := prestate; []) else [t] end\n" ^
     "      fun fxx () = let val f = fx () in\n" ^
     "         if f = [] then [] else f @ fx()\n" ^
     "         end\n" ^
     "   in\n" ^
     "      SOME (fxx())\n" ^
     "   end\n")
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

