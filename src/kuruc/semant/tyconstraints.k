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
*    Global type constraint store.
******************************************************************************)
structure TyConstraints =
struct
   structure T = Type

   type pos = ParseTree.pos
   type constr = T.ty * T.ty * pos

   val constraints = ref [] : constr list ref

   fun add c = 
      constraints := c :: (!constraints)

   fun getConstraints () = List.rev (!constraints)

	fun clearConstraints () = constraints := []

   fun prettyPrint () = 
      List.foldl (fn ((l,r,p),s) => 
           s ^ Error.resolveErrorPos p ^ ": " ^ 
           T.prettyPrint l ^ "\t\t:\t" ^ T.prettyPrint r ^ "\n")
           "" (getConstraints ())

end

