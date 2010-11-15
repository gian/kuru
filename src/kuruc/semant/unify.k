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
                        (map (fn (l,r,p) => 
                                          Error.resolveErrorPos p ^ ":  " ^
                                          T.prettyPrint l ^ " : " ^
                                          T.prettyPrint r) c)

   fun substinconstr tn th l = 
      let
         val _ = Debug.print Debug.debug ("substinconstr: " ^ 
                                          T.prettyPrint tn ^ " for " ^ 
                                          T.prettyPrint th ^ "\n")
         val _ = Debug.print Debug.debug ("===== before subst =====\n") 
         val _ = Debug.print Debug.debug ("\n" ^ ppc l ^ "\n")
         
         val c' = map (fn (a,b,p) => (T.substinty tn th a,
                                      T.substinty tn th b,p)) l

         val _ = Debug.print Debug.debug ("===== after subst =====\n") 
         val _ = Debug.print Debug.debug ("\n" ^ ppc c' ^ "\n")
      in
         c'
      end

   fun substinconstr_rhs tn th l = 
      let
         val c' = map (fn (a,b,p) => (a,
                                    T.substinty tn th b,p)) l
      in
         c'
      end


   fun conzip ([],[],p) = []
     | conzip (h1::t1,h2::t2,p) = (h1,h2,p) :: conzip (t1,t2,p)
     | conzip ([],l,p) = 
         Error.fail ("Encountered type lists of different lengths",p)
     | conzip (l,[],p) =
         Error.fail ("Encountered type lists of different lengths",p)

   fun unify' [] = []
     | unify' ((T.META x,t2,p) :: rest) = 
         if T.equal t2 (T.META x) then unify rest
         else if T.occursin (T.META x) t2 then
               Error.fail ("Circular type constraints",p)
         else (unify (substinconstr (T.META x) t2 rest)) @ [(T.META x,t2,p)]
     | unify' ((t1,T.META x,p) :: rest) = 
         if T.equal t1 (T.META x) then unify rest
         else if T.occursin (T.META x) t1 then
               Error.fail ("Circular type constraints",p)
         else (unify (substinconstr (T.META x) t1 rest)) @ [(T.META x,t1,p)]
     | unify' ((T.NAME s,T.NAME t,p) :: rest) =
         if s = t then unify rest
         else Error.fail ("Type error: " ^ S.name s ^ " != " ^ S.name t,p)
     | unify' ((T.FN(t1,t2),T.FN(t3,t4),p) :: rest) =
	  		unify ((conzip (t1,t3,p)) @ [(t2,t4,p)] @ rest)
     | unify' ((T.TUPLE l1,T.TUPLE l2,p) :: rest) =
         unify ((conzip (l1,l2,p)) @ rest)
     | unify' ((T.CONSTR (l1,lt),T.CONSTR (r1,rt),p) :: rest) =
	  		unify ((conzip (l1,r1,p)) @ [(lt,rt,p)] @ rest)
     | unify' ((T.POLY i, T.POLY j,p) :: rest) =
         if i = j then unify rest else 
            Error.fail ("Type error: Polymorphic unification",p)
     | unify' ((t1,t2,p) :: rest) = 
         Error.fail ("Type error: " ^ T.prettyPrint t1 ^ " != " ^ 
                                      T.prettyPrint t2,p)

   and unify [] = []
     | unify ((t1,t2,p) :: rest) =
       let
          val _ = Debug.print Debug.debug ("Unifying " ^ T.prettyPrint t1 ^ 
                                           " with " ^ T.prettyPrint t2 ^ "\n")
       in
          unify' ((t1,t2,p) :: rest) handle CompileError => unify' rest
       end

   fun apply [] d = d
     | apply ((l,r,p)::t) d = apply t (A.subst_ty_dec l r d)

   fun generalise ve cs =
      let
         val pv = ref 0
         val st = ref S.empty : T.ty S.table ref
         val csr = ref cs

         (* val keys = map (fn (k,Env.VarEntry t) => t) (S.keys ve) *)
         val keys = map (fn (l,r,p) => r) (!csr)

         fun bound [] = Set.empty
           | bound ((l,r,p)::t) = Set.union (T.freevars l,bound t)

         val kfr = bound cs

         fun g pos (T.META i) =
            if not (Set.member (T.META i,kfr)) then
               (case S.look (!st,S.symbol (Int.toString i)) of 
                  SOME t' => t'
                | NONE => 
                  let
                     val p = T.POLY (!pv)
                     val _ = pv := !pv + 1
                     val _ = st := S.enter (!st,S.symbol (Int.toString i),
                                            p)
                     val _ = csr := (substinconstr_rhs (T.META i) p (!csr)) @
                                  [(T.META i, p, pos)] 
                  in
                     p
                  end)
              else T.META i
           | g pos (T.FN (t1,t2)) = T.FN (map (g pos) t1, g pos t2)
           | g pos (T.TUPLE l1) = T.TUPLE (map (g pos) l1)
           | g pos (T.CONSTR (l1,t1)) = T.CONSTR (map (g pos) l1, g pos t1)
           | g pos x = x

         val _ = map (fn (l,r,p) => (pv := 0; (l,g p r,p))) cs
      in
         !csr
      end

end
