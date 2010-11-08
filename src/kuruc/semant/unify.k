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
*    Solve type constraints by unification
******************************************************************************)
structure Unify =
struct
	structure T = Type
   structure S = Symbol
   structure A = TypedAST
   
   fun ppc c = String.concatWith "\n" 
                        (map (fn (l,r) => T.prettyPrint l ^ " : " ^
                                          T.prettyPrint r) c)

          val _ = Debug.print Debug.debug "generalise constraint set:\n"
          val _ = Debug.print Debug.debug "\n"

   fun substinconstr tn th l = 
      let
         val _ = Debug.print Debug.debug ("substinconstr: " ^ 
                                          T.prettyPrint tn ^ " for " ^ 
                                          T.prettyPrint th ^ "\n")
         val _ = Debug.print Debug.debug ("===== before subst =====\n") 
         val _ = Debug.print Debug.debug ("\n" ^ ppc l ^ "\n")
         
         val c' = map (fn (a,b) => (T.substinty tn th a,
                                    T.substinty tn th b)) l

         val _ = Debug.print Debug.debug ("===== after subst =====\n") 
         val _ = Debug.print Debug.debug ("\n" ^ ppc c' ^ "\n")
      in
         c'
      end

   fun substinconstr_rhs tn th l = 
      let
         val c' = map (fn (a,b) => (a,
                                    T.substinty tn th b)) l
      in
         c'
      end


   fun unify' [] = []
     | unify' ((T.META x,t2) :: rest) = 
         if T.equal t2 (T.META x) then unify rest
         else if T.occursin (T.META x) t2 then
                  raise (Fail "Circular constraints")
         else (unify (substinconstr (T.META x) t2 rest)) @ [(T.META x, t2)]
     | unify' ((t1,T.META x) :: rest) = 
         if T.equal t1 (T.META x) then unify rest
         else if T.occursin (T.META x) t1 then
                  raise (Fail "Circular constraints")
         else (unify (substinconstr (T.META x) t1 rest)) @ [(T.META x, t1)]

     | unify' ((T.NAME s,T.NAME t) :: rest) =
         if s = t then unify rest
         else raise (Fail ("Type error: " ^ S.name s ^ " != " ^ S.name t))
     | unify' ((T.FN(t1,t2),T.FN(t3,t4)) :: rest) =
	  		unify ((ListPair.zip (t1,t3)) @ [(t2,t4)] @ rest)
     | unify' ((T.TUPLE l1,T.TUPLE l2) :: rest) =
         unify ((ListPair.zip (l1,l2)) @ rest)
     | unify' ((T.CONSTR (l1,lt),T.CONSTR (r1,rt)) :: rest) =
	  		unify ((ListPair.zip (l1,r1)) @ [(lt,rt)] @ rest)
     | unify' ((T.POLY i, T.POLY j) :: rest) =
         if i = j then unify rest else 
            raise Fail ("Type error: Polymorphic unification")
     | unify' ((t1,t2) :: rest) = 
         raise Fail ("Type error: " ^ T.prettyPrint t1 ^ " != " ^ 
                                      T.prettyPrint t2)

   and unify [] = []
     | unify ((t1,t2) :: rest) =
       let
          val _ = Debug.print Debug.debug ("Unifying " ^ T.prettyPrint t1 ^ 
                                           " with " ^ T.prettyPrint t2 ^ "\n")
       in
          unify' ((t1,t2) :: rest)
       end

   fun apply [] d = d
     | apply ((l,r)::t) d = apply t (A.subst_ty l r d)

   fun generalise cs t =
      let
         val pv = ref 0
         val st = ref S.empty : T.ty S.table ref
         val csr = ref cs

         fun g (T.META i) = 
               (case S.look (!st,S.symbol (Int.toString i)) of 
                  SOME t' => t'
                | NONE => 
                  let
                     val p = T.POLY (!pv)
                     val _ = pv := !pv + 1
                     val _ = st := S.enter (!st,S.symbol (Int.toString i),
                                            p)
                     val _ = csr := (substinconstr_rhs (T.META i) p (!csr)) @
                                       [(T.META i, p)]
                  in
                     p
                  end)
           | g (T.FN (t1,t2)) = T.FN (map  g t1, g t2)
           | g (T.TUPLE l1) = T.TUPLE (map g l1)
           | g (T.CONSTR (l1,t1)) = T.CONSTR (map g l1, g t1)
           | g x = x
      in
         (g t, !csr)
      end

end
