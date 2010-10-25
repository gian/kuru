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
*    Intermediate Representation.
******************************************************************************)
structure Intermediate =
struct
   structure T = Type
   structure S = Symbol
   structure A = TypedAST

   datatype exp = CONST of int
                | RCONST of real
                | NAME of Temp.label
                | TEMP of Temp.temp
                | MEM of exp
                | CALL of Temp.ty * exp * exp
                | SEQ of exp list
                | TUPLE of Temp.ty list * exp list
                | TUPLE_IND of exp * int
                | VOID
   
        and stm = MOVE of exp * exp
                | EXP of exp
                | JUMP of exp * Temp.label list
                | CJUMP of relop * exp * exp * Temp.label
                | CJUMP2 of relop * exp * exp * Temp.label * Temp.label
                | PHI of exp * (exp * Temp.label) list
                | LABEL of Temp.label
                | RET of exp
   
      and relop = EQ | NEQ | LT | GT | LE | GE | ULT | ULE | UGT | UGE
                | REQ | RNEQ

   (* Some built-in calls *)
   fun kstreq l1 l2 = raise Fail ("kstreq not implemented") 

end
