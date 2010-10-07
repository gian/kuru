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
*    Type environment entries
******************************************************************************)
structure Type =
struct

	datatype ty = NAME of Symbol.symbol
	            | FN of ty * ty
					| TUPLE of ty list
					| CONSTR of ty list * ty
					| CLASS of ty Set.set
					| QUANT of ty * ty * ty
					| META of int
					| VAR of Symbol.symbol
					| POLY of int
					| UNKNOWN

	fun pp (NAME s) = Symbol.name s
	  | pp (FN (t1,t2)) = pp t1 ^ " -> " ^ pp t2
	  | pp (TUPLE l) = String.concatWith " * " (map pp l)
	  | pp (CONSTR ([],t2)) = pp t2
	  | pp (CONSTR ([t1],t2)) = pp t1 ^ " " ^ pp t2
	  | pp (CONSTR (t1,t2)) = 
	  			"(" ^ (String.concatWith "," (map pp t1)) ^ ") " ^ pp t2
	  | pp (CLASS l) = "{" ^ (String.concatWith "," (map pp l)) ^ "}"
	  | pp (QUANT (t1,t2,t3)) = pp t1 ^ " " ^ pp t2 ^ " => " ^ pp t3
	  | pp (VAR s) = Symbol.name s
	  | pp (META i) = "?X" ^ Int.toString i 
	  | pp (POLY i) = "'" ^ (String.str (Char.chr (i - (Char.ord #"a"))))
     | pp (UNKNOWN) = "???"

	val prettyPrint = pp

	(* Global counter to allow us to generate fresh type variables *)
	val tvFresh = ref 0
	fun fresh () = (tvFresh := !tvFresh +1; 
						 META (!tvFresh))

   val t_string = NAME (Symbol.symbol "string")
   val t_int    = NAME (Symbol.symbol "int")
   val t_real   = NAME (Symbol.symbol "real")
   val t_char   = NAME (Symbol.symbol "char")
   val t_unit   = NAME (Symbol.symbol "unit")
end



