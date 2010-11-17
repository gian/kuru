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

   val fr = ref 0

   fun fresh () = (fr := !fr + 1; Symbol.symbol ("_cc" ^ Int.toString (!fr)))

   fun vtol f = Vector.foldr (fn (a,b) => a::b) [] f

   fun bind (A.PTUPLE (v,t,p)) = Vector.foldl (fn (e,sl) => sl @ bind e) [] v
     | bind (A.PCONSTR (_,SOME (e,_),_,_)) = bind e
     | bind (A.PID (s,t,p)) = [s]
     | bind (A.PINFIX (p1,_,p2,_,_)) = bind p1 @ bind p2
     | bind x = []

   fun free bnd (A.ELIT _) = []
     | free bnd (A.EID (s,t,p)) = if Set.member (s,bnd) then [] else [s]
     | free bnd (A.ETUPLE (v,t,p)) = 
         Vector.foldl (fn (e,sl) => sl @ free bnd e) [] v
     | free bnd (A.EIF (a,b,c,t,p)) = free bnd a @ free bnd b @ free bnd c
     | free bnd (A.EWHILE (a,b,t,p)) = free bnd a @ free bnd b
     | free bnd (A.EFN (s,e,t,p)) = free (bnd @ s) e
     | free bnd (A.ECASE (e,v,t,p)) =
         free bnd e @
            Vector.foldl 
               (fn ((pa,ex),sl) => sl @ (free (bnd @ (bind pa)) ex))
               bnd v
     | free bnd (A.EAPP (e1,e2,t,p)) = free bnd e1 @ free bnd e2
     | free bnd (A.ERAISE (e,p)) = free bnd e
     | free bnd (A.EHANDLE (pa,e,t,p)) = free (bnd @ (bind pa)) e
     | free bnd (A.ESEQ (v,t,p)) = 
         Vector.foldl (fn (e,sl) => free bnd e @ sl) [] v
     | free bnd (A.ELET (v,e,t,p)) =
      let
         (* FIXME: this probably already needs to be gone *)
         val _ = ()
      in
         free bnd e
      end
     | free bnd (A.EANN (e,t,p)) = free bnd e
     | free bnd (A.ECON (i,NONE,t,p)) = []
     | free bnd (A.ECON (i,SOME (e,_),_,_)) = free bnd e
     | free bnd _ = []
               

   fun convert ((A.DVAL (s,e,t,p)) :: rest) =
      let
         val fr = free [s] e

         val _ = Debug.print 
            Debug.debug ("ClosureConvert: expression: " ^ 
               A.ppe e ^ "\n")

         val _ = Debug.print 
            Debug.debug ("ClosureConvert: free: " ^ 
               String.concatWith ", " (map Symbol.name fr) ^ "\n")
      in
         A.DVAL (s,e,t,p) :: convert rest
      end
     | convert ((A.DVALREC (a,t,p)) :: rest) =
         (A.DVALREC (hd (convert [a]),t,p)) :: convert rest
     | convert ((A.DMUTUAL (v,p)) :: rest) =
         (A.DMUTUAL (Vector.fromList (convert (vtol v)), p)) :: convert rest
     | convert ((A.DNULL p) :: rest) = convert rest
     | convert ((A.DSTRUCTURE (s,c,v,p)) :: rest) =
         (A.DSTRUCTURE (s, c, Vector.fromList (convert (vtol v)), p)) 
            :: convert rest
     | convert ((A.DSTRUCTASSIGN (s,t,p)) :: rest) =
         (A.DSTRUCTASSIGN (s,t,p)) :: convert rest
     | convert (h::rest) = h :: convert rest

end

