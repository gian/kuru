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

   fun substinconstr tn th l = map (fn (a,b) => (T.substinty tn th a,
                                                 T.substinty tn th b)) l

   fun unify [] = []
     | unify ((t1,T.META x) :: rest) = 
         if T.equal t1 (T.META x) then unify rest
         else if T.occursin (T.META x) t1 then
                  raise (Fail "Circular constraints")
         else (unify (substinconstr (T.META x) t1 rest)) @ [(T.META x, t1)]
     | unify ((T.META x,t2) :: rest) = 
         if T.equal t2 (T.META x) then unify rest
         else if T.occursin (T.META x) t2 then
                  raise (Fail "Circular constraints")
         else (unify (substinconstr (T.META x) t2 rest)) @ [(T.META x, t2)]
     | unify ((T.NAME s,T.NAME t) :: rest) =
         if s = t then unify rest
         else raise (Fail ("Type error: " ^ S.name s ^ " != " ^ S.name t))
     | unify ((T.FN(t1,t2),T.FN(t3,t4)) :: rest) =
         unify ((t1,t3) :: (t3,t4) :: rest)
     | unify ((T.TUPLE l1,T.TUPLE l2) :: rest) =
         unify ((ListPair.zip (l1,l2)) @ rest)
     | unify ((t1,t2) :: rest) = 
         raise Fail ("Type error: " ^ T.prettyPrint t1 ^ " != " ^ 
                                      T.prettyPrint t2)

         
end
