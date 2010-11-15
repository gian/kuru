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
structure Polymorphism =
struct
	structure T = Type
   structure S = Symbol
   structure A = TypedAST
   
   fun isPolymorphic e =
      let
         val t = A.getTypeExp e

         fun oc tyT = (case tyT of
				 T.FN(tyT1,tyT2) => (List.all oc tyT1) orelse oc tyT2
			  | T.TUPLE tyT1 => List.all oc tyT1
			  | T.CONSTR (tyT1,t2) => List.all oc tyT1 orelse oc t2
			  | T.META x => true
           | T.VAR x => true
           | T.POLY x => true
			  | _ => false)

         val p = oc t

         val _ = if p then Debug.print 
               Debug.debugmore ("is polymorphic: " ^ A.ppe e ^ "\n") 
            else
               Debug.print Debug.debugmore 
                  ("is not polymorphic: " ^ A.ppe e ^ "\n")
      in
         p
      end

   (* FIXME: this is not quite correct - 
      it may end up capturing polymorphic nested functions. *)
   fun monomorphise (T.FN([a],b)) (T.FN ([a'],b')) e = 
      let
         val _ = Debug.print Debug.internal
            ("Monomorphising expression: " ^ A.ppe e ^ ".\nInstantiating: " ^
               T.prettyPrint a ^ " -> " ^ T.prettyPrint b ^ " to " ^
               T.prettyPrint a' ^ " -> " ^ T.prettyPrint b' ^ "\n")
         val e' = A.subst_ty_exp a a' e
         val e'' = A.subst_ty_exp b b' e'
      in
         e''
      end
     | monomorphise (t1 as T.FN([a],b)) t2 e = 
      let
         val _ = Debug.print Debug.internal
            ("Monomorphising expression: " ^ A.ppe e ^ ".\nInstantiating: " ^
               T.prettyPrint a ^ " -> " ^ T.prettyPrint b ^ " to " ^
               (T.prettyPrint (T.substinty a t2 t1)) ^ "\n")
         val e' = A.subst_ty_exp a t2 e
      in
         e'
      end
     | monomorphise _ _ e = 
         (Debug.print Debug.warning 
            ("Polymorphism.monomorphise applied to non-function type.\n"); e)
end
