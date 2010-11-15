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
*    Monomorphise TypedASTs, instantiating polymorphic functions and
*    resolving type classes to instances.
******************************************************************************)
structure Monomorphise =
struct
   structure A = TypedAST
   structure T = Type

   (** TODO: Apply the value restriction here *)
   fun monomorphise prog =
   let
      fun mexp (A.ETUPLE (v,t,p)) =
      let
         val v' = Vector.map mexp v
      in
         A.ETUPLE (v,t,p)
      end
        | mexp (A.EIF (a,b,c,t,p)) = A.EIF (mexp a,mexp b,mexp c,t,p)
        | mexp (A.EWHILE (a,b,t,p)) = A.EWHILE (mexp a, mexp b, t, p)
        | mexp (A.EFN (s,e,t,p)) = A.EFN (s,mexp e,t,p)
        | mexp (A.ECASE (e,v,t,p)) =
            A.ECASE (mexp e, Vector.map (fn (p,e) => (p,mexp e)) v, t, p)
        | mexp (A.EAPP (e1,e2,t,p)) =
      if Polymorphism.isPolymorphic e1 then
      let
         val te1 = A.getTypeExp e1
         val te1' = Constrain.inst te1
         val te2 = A.getTypeExp e2
         val tx = T.fresh ()
         val tf = T.FN ([A.getTypeExp e2], T.fresh ())
         val cs = [(te1',tf,p)]
         val cs' = Unify.unify cs
         
         fun apply [] e = e
           | apply ((l,r,p)::t) e = apply t (A.subst_ty_exp l r e)

         val e1' = apply cs' e1
         val rt = case A.getTypeExp e1' of
                     T.FN (_,b) => b
                   | _ => Error.fail ("Invalid non-function for monomorphisation",p)

         val _ = Debug.print Debug.debug 
            ("Monomorphise: " ^ A.ppe e1' ^ "\n" ^ 
             "e2: " ^ A.ppe e2 ^ "\n" ^ 
             "te1: " ^ A.ppty te1 ^ "\n" ^ 
             "te2: " ^ A.ppty te2 ^ "\n")
      in
         A.EAPP (A.EINST(e1',te1,tf),e2,rt,p)
      end
      else A.EAPP (e1,e2,t,p)
        | mexp (A.ERAISE (e,p)) = A.ERAISE (mexp e,p)
        | mexp (A.ESEQ (v,t,p)) = A.ESEQ (Vector.map mexp v, t, p)
        | mexp x = x

      fun mstm (A.DVAL (s,e,t,p)) = A.DVAL (s,mexp e,t,p)
        | mstm (A.DVALREC (a,t,p)) = A.DVALREC (mstm a, t, p)
        | mstm (A.DMUTUAL (v,p)) = A.DMUTUAL (Vector.map mstm v, p)
        | mstm x = x
   in
      prog 
   end
end

