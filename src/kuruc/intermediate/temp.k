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
*    Temporaries within the IR. 
******************************************************************************)
structure Temp =
struct
   structure S = Symbol

   datatype ty = i1
               | i8
               | i16
               | i32
               | i64
               | label of string

   type temp = int * ty
   type label = temp

   val lc = ref 0
   val tc = ref 0

   (* FIXME: There's something broken here.
      we use symbol hashes to generate temporaries,
      but also just increment tc.  Should check for collisions,
      or come up with a better mechanism for temporaries. *)
   fun newtemp  t = (tc := !tc + 1; (!tc,t))
   fun newlabel () = newtemp (label ("l" ^ Int.toString (lc := !lc + 1; !lc)))

   fun labelFromString s = (0,label s)
   fun tempFromSymbol s t = (S.hash s, t)
   fun getType (_,t) = t : ty
end

