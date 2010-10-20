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

   fun freevars (NAME s) = Set.empty
     | freevars (FN (t1,t2)) = Set.union (freevars t1, freevars t2)
     | freevars (TUPLE l) = 
         List.foldl (fn (a,b) => Set.union(b,freevars a)) Set.empty l
     | freevars (CONSTR (l,t)) = 
         Set.union (List.foldl (fn (a,b) => Set.union(b,freevars a)) Set.empty l,
                    freevars t)
     | freevars (META i) = Set.singleton (META i)
     | freevars _ = []

   fun substinty (NAME s) th (NAME t) = if s = t then th else NAME t
     | substinty tn th (FN (t1,t2)) = FN (substinty tn th t1,
                                          substinty tn th t2)
     | substinty tn th (TUPLE l) = TUPLE (map (substinty tn th) l)
     | substinty tn th (CONSTR (l,t)) = CONSTR (map (substinty tn th) l,
                                                substinty tn th t)
     | substinty (META i) th (META j) = if i = j then th else META j
     | substinty (VAR s) th (VAR t) = if s = t then th else VAR t
     | substinty (POLY i) th (POLY j) = if i = j then th else POLY j
     | substinty tn th t = t 

   fun equal (NAME s) (NAME t) = s = t
     | equal (FN (t1,t2)) (FN (t3,t4)) = equal t1 t3 andalso equal t2 t4
     | equal (TUPLE l1) (TUPLE l2) = 
         List.all (fn (a,b) => equal a b) (ListPair.zip (l1,l2))
     | equal (CONSTR (l1,t1)) (CONSTR (l2,t2)) = 
         (List.all (fn (a,b) => equal a b) (ListPair.zip (l1,l2))) andalso
         equal t1 t2
     | equal (META i) (META j) = i = j
     | equal (VAR s) (VAR t) = s = t
     | equal (POLY i) (POLY j) = i = j
     | equal _ _ = false

	fun occursin (META tyX) tyT =
		let
			fun oc tyT = (case tyT of
				 FN(tyT1,tyT2) => oc tyT1 orelse oc tyT2
			  | TUPLE tyT1 => List.all oc tyT1
			  | CONSTR (tyT1,t2) => List.all oc tyT1 orelse oc t2
			  | META x => (x = tyX)
			  | _ => false)
     in
        oc tyT
     end
	  | occursin _ _ = raise Fail "Non-UVar argument to occursin"

	fun pp (NAME s) = Symbol.name s
     | pp (FN (f as FN _,t2)) = "(" ^ pp f ^ ") -> " ^ pp t2
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
	  | pp (POLY i) = "'" ^ (String.str (Char.chr (i + (Char.ord #"a"))))
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

   val t_list   = NAME (Symbol.symbol "list")
end



