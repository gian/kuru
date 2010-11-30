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
   structure I = Intermediate
   structure T = Type
   structure S = Symbol

   val env = ref S.empty : I.ir list S.table ref
   val lifted = ref [] : (S.symbol * I.ir) list ref

   (* Allows lookup of a function symbol's environment.
      getEnv never fails - so it is safe to lookup non-function symbols too. *)
   fun setEnv f e = env := S.enter (!env, f, e)
   fun getEnv f =
      case S.look (!env,f) of NONE => [] | SOME x => x

   val frn = ref 0
   fun fresh () = S.symbol (frn := !frn + 1; "_cc" ^ Int.toString (!frn))

   val globals = ref S.empty : unit S.table ref
   fun addGlobal s = globals := S.enter (!globals, s, ())
   fun isGlobal s = 
      case S.look (!globals,s) of
         NONE => false
       | SOME _ => true


   fun exSym (I.N (I.IDENT k,_,_,_,_)) = k
     | exSym (I.N (_,_,_,_,p)) = 
         Error.bug ("ClosureConvert.exSym", "Called on invalid symbol",p)

   fun repEnv gs l n =
   let
      val e = ref 0
      fun inc () = !e before e := !e + 1

      fun repEnv' s (n as I.N (I.IDENT k,_,t,m,p)) = 
            if s = k then I.N (I.ENV (inc (),gs),[],t,[],p) else n
        | repEnv' s (I.N (opr,l,t,m,p)) = 
            I.N (opr, map (repEnv' s) l, t, m, p)
   in
      repEnv' l n
   end

   (* Substitute anything in the free set 'fr' with 
      environment accesses. *)
   fun lift s [] n = lifted := !lifted @ [(s,n)]
     | lift s (h::t) n = 
         lift s t (repEnv s (exSym h) n)

   fun names b (n as I.N (I.IDENT k,_,T.FN _,_,_)) = []
     | names b (n as I.N (I.IDENT k,_,_,_,_)) = 
      if (List.exists (fn x => x = k) b) orelse
            isGlobal k then [] else [n]
     | names b (n as I.N (I.BIND k,[e],_,_,_)) =
         names (k :: b) e
     | names b (n as I.N (I.MATCH, [a,c],_,_,_)) =
         names (b @ (map exSym (names b a))) c
     | names b (I.N (_,l,_,_,_)) = 
         List.concat (map (names b) l)

   fun liftFun (n as I.N (I.FN, [s,b], t, m, p)) = 
   let
      val _ = Debug.print Debug.debug
         ("ClosureConvert: Lifting function: " ^ I.prettyPrint n ^ "\n")
      val bound = [exSym s]
      val b' = liftFun b
      val fr = names bound b'
      val k = fresh ()
      val _ = lift k fr (I.N (I.FUNDEC k, [s,b'], t, m, p))
      val _ = setEnv k fr
   in
      I.N (I.IDENT k,[],t,[],p)
      (* I.N (I.CLOSURE, [I.N (I.IDENT k,[],t,[],p)] @ fr, t, [], p) *)
   end
     | liftFun (I.N (I.IDENT k, _, t as T.FN ([it],ot), m, p)) =
   let
      val fr = getEnv k
      val _ = Debug.print Debug.debug
         ("ClosureConvert: liftFun IDENT " ^ S.name k ^ "\n")
   in
      I.N (I.CLOSURE, [I.N (I.IDENT k,[],t,[],p)] @ fr, t, [], p)
   end
     | liftFun (x as I.N (I.IDENT k, c, t, m, p)) =
   let
      val _ = Debug.print Debug.debug
         ("ClosureConvert.liftFun: Non-FN Ident: " ^ S.name k ^ " : " ^
            T.prettyPrint t ^ "\n")
   in
      x
   end
     | liftFun (I.N (I.PROGRAM, l, t, m, p)) =
   let
      val l' = map liftFun l

      val lifted' = 
         map (fn (_,i) => i) (!lifted)
   in
      I.N (I.PROGRAM, lifted' @ l', t, m, p)
   end
     | liftFun (I.N (I.BIND k, [I.N (I.EXTERN s,_,t,_,_)], _,_,p)) =
   let
      val _ = lift k [] (I.N (I.ALIAS (k,s,t),[],t,[],p))
   in
      I.N (I.BIND k,[],T.UNKNOWN,[],p)
   end
     | liftFun (I.N (I.APP, [a,b], t, m, p)) =
   let
      val a' = liftFun a
      val b' = liftFun b

      val t' = I.getType a'

      val k = 
         case a' of 
            I.N (I.IDENT k,_,_,_,_) => 
            let
               val fr = getEnv k
            in
             I.N (I.APP,
               [
                  I.N (I.CLOSURE, [I.N (I.IDENT k,[],t',[],p)] @ fr, t', [], p),
                  b'
               ], t, m, p)
            end
          | x => 
             I.N (I.APP, [a',b'], t, m, p)
   in
      k
   end
     | liftFun (I.N (I.BIND s, [e], t, m, p)) =
   let
      val _ = Debug.print Debug.debug
         ("ClosureConvert: BIND " ^ S.name s ^ " = " ^ I.prettyPrint e ^ "\n")

      val e' = liftFun e

      val _ = Debug.print Debug.debug
         ("ClosureConvert: (post) BIND " ^ S.name s ^ " = " ^
            I.prettyPrint e' ^ "\n")

      val ins = 
         case e' of 
            I.N (I.IDENT k,_,_,_,_) => I.N (I.ALIAS (s,k,t),[],t,[],p)
          | _ => I.N (I.BIND s, [e'], t, m, p)

   in
      ins
   end
     | liftFun (I.N (opr,l,t,m,p)) = 
         I.N (opr, map liftFun l, t, m, p)

   (* FIXME: Need to add a top-level that does not change the names of
      bound functions, but instead just converts 
       BIND nodes with FN RHS to FUNDEC nodes, and then recurses on the body
       with a function that will actually hoist functions out to top level. *)

   fun convert p = 
   let
      val p' = liftFun p
      val _ = Debug.print Debug.debug 
         ("ClosureConvert: Lifted:\n" ^ 
            (String.concatWith "\n"
               (map (fn (s,i) => "BIND " ^ S.name s ^ " = " ^ I.prettyPrint i)
                  (!lifted))) ^ "\n")
   in
      p'
   end
end

