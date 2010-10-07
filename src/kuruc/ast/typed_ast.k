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
*    A typed AST into which ParseTree trees are transformed.
******************************************************************************)
structure TypedAST =
struct
	structure T = Type
   structure S = Symbol

	type pos = ParseTree.pos

   datatype tval = LINT of int * pos
                 | LREAL of real * pos
                 | LCHAR of char * pos
                 | LSTRING of string * pos
                 | LUNIT of pos

   datatype tpat = PCONST of texp * T.ty * pos
                 | PLIST of tpat vector * T.ty * pos
                 | PTUPLE of tpat vector * T.ty * pos
                 | PCONSTR of S.symbol * tpat option * T.ty * pos
                 | PID of S.symbol * T.ty * pos 

   and      texp = ELIT of tval * T.ty * pos
	              | EID of S.symbol * T.ty * pos
                 | ELIST of texp vector * T.ty * pos
                 | ETUPLE of texp vector * T.ty * pos
                 | EIF of texp * texp * texp * T.ty * pos
                 | EWHILE of texp * texp * T.ty * pos
                 | EFN of S.symbol * texp * T.ty * pos
                 | ECASE of texp * (tpat * texp) vector * T.ty * pos
                 | EAPP of texp * texp * T.ty * pos
                 | EEXN of texp * T.ty * pos
                 | ERAISE of texp * pos
                 | EHANDLE of tpat * texp * T.ty * pos
                 | ESEQ of texp vector * T.ty * pos
                 | ELET of tast list * texp * T.ty * pos
                 | EANN of texp * T.ty * pos
                 
   and      tast = DVAL of S.symbol * texp * T.ty * pos
                 | DVALREC of tast * T.ty * pos
                 | DMUTUAL of tast vector * pos
                 | DSTRUCTURE of S.symbol * tast vector * pos
                 | DSTRUCTASSIGN of S.symbol * S.symbol * pos
                 | DNULL of pos (* For things we've removed *)

   val ppty = T.pp
   fun vtol f = Vector.foldr (fn (a,b) => a::b) [] f

   fun ppv (LINT(i,p)) = Int.toString i
     | ppv (LREAL(r,p)) = Real.toString r
     | ppv (LCHAR(c,p)) = "#\"" ^ String.str (c) ^ "\""
     | ppv (LSTRING(s,p)) = "\"" ^ s ^ "\""
     | ppv (LUNIT _) = "()"

   fun ppp (PCONST(e,t,p)) = ppe e ^ " : " ^ ppty t
     | ppp (PLIST(v,ty,p)) = "[" ^ (String.concatWith "," (map ppp (vtol v))) ^ "]"
     | ppp (PTUPLE(v,ty,p)) = "(" ^ (String.concatWith "," (map ppp (vtol v))) ^ ")"
     | ppp (PCONSTR(s,NONE,ty,p)) = "???constr???"
     | ppp (PCONSTR(s,SOME cp,ty,p)) = "???constr???"
     | ppp (PID(s,t,p)) = S.name s ^ " : " ^ ppty t 

   and ppe (ELIT(tv,ty,p)) =  
                             ppv tv ^ " : " ^ ppty ty
     | ppe (EID(s,ty,p)) = S.name s ^ "@" ^ Int.toString p ^ " : " ^ ppty ty
     | ppe (ELIST(v,ty,p)) = "[" ^ (String.concatWith "," (map ppe (vtol v))) ^ "]"
     | ppe (ETUPLE(v,ty,p)) = "(" ^ (String.concatWith "," (map ppe (vtol v))) ^ ")"
     | ppe (EFN(s,e,ty,p)) = "EFN@" ^ Int.toString p ^ "[" ^ S.name s ^ 
                             " => " ^ ppe e ^ "] : " ^ ppty ty
     | ppe (ECASE(e1,m,ty,p)) = "ECASE@" ^ Int.toString p ^ " : " ^ ppty ty ^ " [\n" ^
                                (String.concatWith "\n" (map
                                                         (fn (p,e) => ppp p ^ " => " ^ ppe e) (vtol m)))
                                ^ "]\n"
     | ppe (EAPP(e1,e2,ty,p)) = "EAPP@" ^ Int.toString p ^ 
                                "[" ^ ppe e1 ^ " (" ^ ppe e2 ^ ")] : " ^ 
                                ppty ty
     | ppe _ = "???texp???"


   fun prettyPrint [] = ""
     | prettyPrint ((DNULL _)::t) = prettyPrint t
     | prettyPrint ((DVAL(s,e,ty,p))::t) = "DVAL@" ^ Int.toString p ^ " " ^
                                        S.name s ^ " : " ^ ppty ty ^ " = " ^ 
                                        ppe e ^ "\n" ^ prettyPrint t
     | prettyPrint ((DVALREC(l,ty,p))::t) = "DVALREC@" ^ Int.toString p ^ " " ^ 
                                            (prettyPrint [l]) ^"\n"
                                            ^ prettyPrint t
     | prettyPrint ((DMUTUAL(v,p))::t) = "DMUTUAL@" ^ Int.toString p ^ " [\n" ^ 
                                          prettyPrint (vtol v) ^ "]\n" ^ 
                                          prettyPrint t
     | prettyPrint (_::t) = "???\n" ^ prettyPrint t
end


