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

   fun exSym (I.N (I.IDENT k,_,_,_,_)) = k
     | exSym (I.N (_,_,_,_,p)) = 
         Error.bug ("ClosureConvert.exSym", "Called on invalid symbol",p)

   fun repEnv s (n as I.N (I.IDENT k,_,t,m,p)) = 
         if s = k then I.N (I.ENV k,[],t,[],p) else n
     | repEnv s (I.N (opr,l,t,m,p)) = 
         I.N (opr, map (repEnv s) l, t, m, p)

   (* Substitute anything in the free set 'fr' with 
      environment accesses. *)
   fun lift s [] n = lifted := !lifted @ [(s,n)]
     | lift s (h::t) n = 
         lift s t (repEnv (exSym h) n)

   fun names b (n as I.N (I.IDENT k,_,_,_,_)) = 
      if List.exists (fn x => x = k) b then [] else [n]
     | names b (n as I.N (I.BIND k,[e],_,_,_)) =
         names (k :: b) e
     | names b (I.N (_,l,_,_,_)) = 
         List.concat (map (names b) l)

   fun liftFun (I.N (I.FN, [s,b], t, m, p)) = 
   let
      val bound = [exSym s]
      val b' = liftFun b
      val fr = names bound b'
      val k = fresh ()
      val _ = lift k fr (I.N (I.FN, [s,b'], t, m, p))
      val _ = setEnv k fr
   in
      I.N (I.CLOSURE, [I.N (I.IDENT k,[],t,[],p)] @ fr, t, [], p)
   end
     | liftFun (I.N (opr,l,t,m,p)) = 
         I.N (opr, map liftFun l, t, m, p)

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

