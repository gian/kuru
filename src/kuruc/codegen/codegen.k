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
*    Code generate LLVM output from IR. 
******************************************************************************)
structure CodeGen =
struct
   structure I = Intermediate
   structure T = Translate

   val j = String.concat
   val jsp = String.concatWith " "
   val jnl = String.concatWith "\n"
   val jc = String.concatWith ", "
   val dquote = "\""

   fun ind 0 = ""
     | ind n = "   " ^ ind n

   fun ppty Temp.i1 = "i1"
     | ppty Temp.i8 = "i8"
     | ppty Temp.i16 = "i16"
     | ppty Temp.i32 = "i32"
     | ppty Temp.i64 = "i64"
     | ppty Temp.r32 = "r32"
     | ppty Temp.r64 = "r64"
     | ppty (Temp.label _) = raise Fail "CodeGen: taking type of label"
     | ppty Temp.string_ptr = "i32*"
     | ppty Temp.void = "void"
     | ppty (Temp.fn_ptr (t1,t2)) = j [ppty t2,
                                       " (",
                                       ppty t1,
                                       ") *"]
     | ppty (Temp.aggr l) = j ["{ ",
                               jc (map ppty l),
                               " }"]
   val ppt = Temp.prettyPrint
   val ppt_np = Temp.prettyPrintNoPrefix 

   fun cg_frag (T.FSTRING (l,s)) = j [ppt_np l, ": ", dquote, s, dquote]
     | cg_frag (T.FFN (l,exps,stms,t)) =
         j [
            jsp ["define fastcc",ppty t,ppt l,"("],
            cg_param

         ]

   fun codegen (stms, frags) =
      jnl [
         jnl (map cg_frag frags),
         jnl (map (cg_stm 1) stms)
      ]
end
