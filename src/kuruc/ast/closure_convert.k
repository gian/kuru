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
*    Perform closure conversion to force all functions to the top level
*  e.g.:
*    val rec p = fn x => 
*       let
*          val z = 10
*          val f = fn y => x + y + z
*       in
*          f 23
*       end
*   -->
*    val _z1 = 10
*    and _f1 = fn _x1 => fn y => _x1 + y + _z1
*    and rec p = fn x  => _f1 x 23
******************************************************************************)
structure ClosureConvert =
struct
   structure A = TypedAST
   structure S = Symbol
   structure D = Debug
   structure T = Type

   val fr = ref 0

   fun fresh () = (fr := !fr + 1; Symbol.symbol ("_cc" ^ Int.toString (!fr)))

	val lifted = ref [] : A.tast list ref

	fun addLiftedFn d = lifted := !lifted @ [d]

   fun vtol f = Vector.foldr (fn (a,b) => a::b) [] f

   fun bind (A.PTUPLE (v,t,p)) = Vector.foldl (fn (e,sl) => sl @ bind e) [] v
     | bind (A.PCONSTR (_,SOME (e,_),_,_)) = bind e
     | bind (A.PID (s,t,p)) = [s]
     | bind (A.PINFIX (p1,_,p2,_,_)) = bind p1 @ bind p2
     | bind x = []

   fun binddec (A.DVAL (s,e,t,p)) = [s]
     | binddec (A.DVALREC (a,_,_)) = binddec a
     | binddec _ = [] (* FIXME *)

   fun free bnd (A.EID (s,t,p)) = if Set.member (s,bnd) then [] else [(s,t)]
     | free bnd (A.ETUPLE (v,t,p)) = 
         Vector.foldl (fn (e,sl) => sl @ free bnd e) [] v
     | free bnd (A.EIF (a,b,c,t,p)) = free bnd a @ free bnd b @ free bnd c
     | free bnd (A.EWHILE (a,b,t,p)) = free bnd a @ free bnd b
     | free bnd (A.EFN (s,e,t,p)) = free (bnd @ s) e
     | free bnd (A.ECASE (e,v,t,p)) =
         free bnd e @
            Vector.foldl 
               (fn ((pa,ex),sl) => sl @ (free (bnd @ (bind pa)) ex))
               [] v
     | free bnd (A.EAPP (e1,e2,t,p)) = free bnd e1 @ free bnd e2
     | free bnd (A.ERAISE (e,p)) = free bnd e
     | free bnd (A.EHANDLE (pa,e,t,p)) = free (bnd @ (bind pa)) e
     | free bnd (A.ESEQ (v,t,p)) = 
         Vector.foldl (fn (e,sl) => free bnd e @ sl) [] v
     | free bnd (A.ELET (v,e,t,p)) =
      let
         val bnd' = 
            Vector.foldl (fn (d,b) => b @ binddec d) bnd v
      in
         free bnd' e
      end
     | free bnd (A.EANN (e,t,p)) = free bnd e
     | free bnd (A.ECON (i,NONE,t,p)) = []
     | free bnd (A.ECON (i,SOME (e,_),_,_)) = free bnd e
     | free bnd _ = []
              

   fun liftExp (A.ETUPLE (v,t,p)) =
   	   A.ETUPLE (Vector.map liftExp v, t, p)
     | liftExp (A.EIF (a,b,c,t,p)) = 
     	   A.EIF (liftExp a, liftExp b, liftExp c, t, p)
     | liftExp (A.EWHILE (a,b,t,p)) = 
     	   A.EWHILE (liftExp a, liftExp b, t, p)
     | liftExp (A.ECASE (e,v,t,p)) =
     	   A.ECASE (liftExp e, Vector.map (fn (p,e) => (p, liftExp e)) v, t, p)
     | liftExp (A.EAPP (e1,e2,t,p)) = 
     	   A.EAPP (liftExp e1, liftExp e2, t, p)
     | liftExp (A.ERAISE (e,p)) =
     	   A.ERAISE (liftExp e, p)
     | liftExp (A.EHANDLE (pa,e,t,p)) = 
     	   A.EHANDLE (pa, liftExp e, t, p)
     | liftExp (A.ESEQ (v,t,p)) =
     	   A.ESEQ (Vector.map liftExp v, t, p)
     | liftExp (A.ELET (v,e,t,p)) =
     	   A.ELET (v, liftExp e, t, p)
     | liftExp (A.EANN (e,t,p)) = 
     	   A.EANN (liftExp e, t, p)
     | liftExp (A.ECON (i,SOME (e,t1),t2,p)) =
         A.ECON (i,SOME (liftExp e,t1),t2,p)
     | liftExp (f as A.EFN ([s],e,t,p)) = 
       let
          val (it,ot) = 
            case t of T.FN ([x],t) => (x,t)
                    | _ => (T.UNKNOWN,T.UNKNOWN)

          val fr = free [] f
          (* fr is the set of things that need to be passed as parameters
             to the lifted function *)
      
          val e' = liftExp e

          (* We're going to lift this anonymous function to be a top-level 
             one named nid *)
          val nid = fresh ()

          val nit = (map (fn (a,b) => b) fr)
          val ns = (map (fn (a,b) => a) fr) 

          val nt = 
            List.foldr 
               (fn (it,ot) =>
                  T.FN ([it],ot)) ot nit

          val _ = D.print D.debug ("ClosureConvert: lifted ty: " ^ A.ppty nt ^ "\n")

          val fty = T.FN ([it],ot)
          val ec = A.ECLOSURE (nid,fr,T.CLOSURE (fty :: nit),p)

          val d = A.DVAL (nid,
                          A.EFN ([S.symbol "env",s],
                                 e',
                                 T.FN([T.CLOSURE (fty :: nit), it],ot),
                                 p), 
                          T.FN([T.CLOSURE (fty :: nit), it],ot),
                          p)

          val _ = addLiftedFn d

          val _ = D.print D.debug ("ClosureConvert: Lifted fn: " ^ A.ppe f ^ "\n")
          val _ = D.print D.debug ("ClosureConvert: Need to add params: " ^ 
                       String.concatWith ", " (map (fn (a,_) => S.name a) fr) ^"\n")
          val _ = D.print D.debug ("ClosureConvert: Lifted fn is: fn " ^ 
                     S.name s ^ " => " ^  A.ppe e' ^ "\n")

       in
          A.EAPP (A.EID (nid,T.FN([T.CLOSURE (fty :: nit),it],ot),p),
                  ec,
                  T.FN ([it],ot),
                  p)
       end
     | liftExp e = e

   fun convert' ((A.DVAL (s,A.EFN (f,e,t2,p2),t1,p1))) =
         A.DVAL (s,A.EFN (f,liftExp e,t2,p2),t1,p1)
     | convert' ((A.DVAL (s,e,t,p))) =
         A.DVAL (s,liftExp e,t,p) 
     | convert' ((A.DVALREC (a,t,p))) =
         (A.DVALREC (convert' a,t,p)) 
     | convert' ((A.DMUTUAL (v,p))) =
         (A.DMUTUAL (Vector.fromList (map convert' (vtol v)), p)) 
     | convert' ((A.DSTRUCTURE (s,c,v,p))) =
         (A.DSTRUCTURE (s, c, Vector.fromList (map convert' (vtol v)), p)) 
     | convert' ((A.DSTRUCTASSIGN (s,t,p))) =
         (A.DSTRUCTASSIGN (s,t,p)) 
     | convert' h = h 

   fun convert l =
      List.foldl (
         fn (d,d') =>
            let
               val _ = D.print D.debug
                  ("Closure converting: " ^ A.prettyPrint [d] ^ "\n")
               val d'' = convert' d
               val li = !lifted
               val _ = lifted := []
            in
               d' @ li @ [d'']
            end) [] l
end

