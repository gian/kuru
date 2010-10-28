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
               | r32
               | r64
               | label of string
               | global of string
               | string_ptr
               | void
               | fn_ptr of ty * ty
               | aggr of ty list

   type temp = int * ty
   type label = temp

   val lc = ref 0
   val tc = ref 0
   val gc = ref 0

   fun newtemp  t = (tc := !tc + 1; (!tc,t))
   fun newlabel () = newtemp (label ("l" ^ Int.toString (lc := !lc + 1; !lc)))
   fun newglobal () = newtemp (global ("g" ^ Int.toString (gc := !gc + 1; !gc)))

   fun labelFromString s = (0,label s)
   (* fun tempFromSymbol s t = (S.hash s, t) *)
   fun getType (_,t) = t : ty

   fun prettyPrint (i,label s) = "%" ^ s
     | prettyPrint (i,global s) = "@" ^ s
     | prettyPrint (i,_) = "%t" ^ Int.toString i

   fun prettyPrintNoPrefix (i,label s) = s
     | prettyPrintNoPrefix (i,global s) = s
     | prettyPrintNoPrefix (i,_) = "t" ^ Int.toString i



end

