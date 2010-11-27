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
*    Internal abstract representation of LLVM assembly.
******************************************************************************)
structure Asm =
struct
   structure T = Type
   structure S = Symbol
   structure A = TypedAST



   datatype exp = CONST of int
                | RCONST of real
                | BCONST of bool
                | EXN of string * exp list
                | NAME of Temp.label
                | TEMP of Temp.temp
                | MEM of exp
                | CMP of relop * exp * exp
                | CALL of Temp.ty * exp * exp
                | SEQ of exp list
                | TUPLE of Temp.ty list * exp list
                | TUPLE_IND of exp * int
                | TUPLE_INS of exp * exp * int (* exp1[int] <- exp2 *)
                | ELEM_PTR of Temp.ty * exp * int list
                | UNDEF of Temp.ty
                | BINOP of string * exp * exp
                | BUILTIN of string * Temp.ty
                | ALLOCA of Temp.ty
                | MALLOC of Temp.ty * int
                | LOAD of exp
                | BITCAST of exp * Temp.ty
                | VOID
                | NULL of Temp.ty
                | ALIAS of exp
                | ENV of int * Temp.ty
   
        and stm = MOVE of exp * exp
                | EXP of exp
                | JUMP of exp * Temp.label list
                | CJUMP of exp * Temp.label * Temp.label
                | PHI of exp * Temp.ty * (exp * Temp.label) list
                | LABEL of Temp.label
                | RET of exp
                | STORE of exp * exp
                | UNWIND
                | COMMENT of string
                | DECLARE of Temp.ty * string * Temp.ty
                | FUNDEC of Temp.ty * string * Temp.temp * stm list
                | SCONST of Temp.label * string
                | UNREACHABLE
   
      and relop = EQ | NEQ | LT | GT | LE | GE | ULT | ULE | UGT | UGE
                | REQ | RNEQ

   (* Some built-in calls *)
   fun kstreq l1 l2 p = Error.bug("Intermediate","kstreq not implemented",p) 

   val ppt = Temp.prettyPrint

   fun ppr EQ = "EQ"
     | ppr NEQ = "NEQ"
     | ppr LT = "LT" 
     | ppr GT = "GT" 
     | ppr LE = "LE" 
     | ppr GE = "GE" 
     | ppr ULT = "ULT" 
     | ppr ULE = "ULE" 
     | ppr UGT = "UGT" 
     | ppr UGE = "UGE" 
     | ppr REQ = "REQ" 
     | ppr RNEQ = "RNEQ" 

   fun ppe (CONST i) = Int.toString i
     | ppe (RCONST i) = Real.toString i
     | ppe (BCONST true) = "true"
     | ppe (BCONST false) = "false"
     | ppe (NAME l) = ppt l
     | ppe (TEMP t) = ppt t
     | ppe (MEM e) = "MEM " ^ ppe e
     | ppe (CALL (t,e1,e2)) = "CALL " ^ ppe e1 ^ " (" ^ ppe e2 ^ ")"
     | ppe (SEQ l) = "SEQ (" ^ String.concatWith "; " (map ppe l) ^ ")"
     | ppe (TUPLE (tl,el)) = "TUPLE (" ^ String.concatWith ","
                                             (map ppe el) ^ ")"
     | ppe (TUPLE_IND (e,i)) = ppe e ^ "[" ^ Int.toString i ^ "]"
     | ppe (TUPLE_INS (e1,e2,i)) = ppe e1 ^ "[" ^ Int.toString i ^ "] <- " ^
                                     ppe e2
     | ppe (ELEM_PTR (t,e,i)) = "ELEM_PTR [" ^ ppe e ^ ", " ^ 
            (String.concatWith ", " (map Int.toString i)) ^ "]"
     | ppe (UNDEF _) = "UNDEF"
     | ppe VOID = "void"
     | ppe (BINOP (s,a,b)) = "BINOP " ^ s ^ " [" ^ ppe a ^ ", " ^ ppe b ^ "]"
     | ppe (BUILTIN (s,t)) = "BUILTIN " ^ s
     | ppe (ALLOCA t) = "ALLOCA"
     | ppe (CMP (r,e1,e2)) = "CMP " ^ ppr r ^ " " ^
                                            ppe e1 ^ " " ^
                                            ppe e2 
     | ppe _ = "???"

   fun ppstm (MOVE (e1,e2)) = ppe e1 ^ " <- " ^ ppe e2
     | ppstm (EXP e) = "EXP " ^ ppe e
     | ppstm (JUMP (e,ll)) = "JUMP " ^ ppe e
     | ppstm (CJUMP (e,l1,l2)) = "CJUMP " ^ ppe e ^ " " ^
                                       ppt l1 ^ " " ^
                                       ppt l2
     | ppstm (PHI (e1, t, l)) = ppe e1 ^ " <- PHI [" ^
                              String.concatWith ", " (
                                map (fn (e,ll) => 
                                   ppe e ^ " " ^ ppt ll) l) ^
                                           "]"
     | ppstm (LABEL l) = ppt l ^ ":"
     | ppstm (RET e) = "RET " ^ ppe e
     | ppstm (UNWIND) = "UNWIND"
     | ppstm (STORE (e1,e2)) = "STORE " ^ ppe e2 ^ " <- " ^ ppe e1
     | ppstm (COMMENT s) = "; " ^ s
     | ppstm (UNREACHABLE) = "unreachable"
     | ppstm (DECLARE (ot,n,it)) = "DECLARE " ^ n
     | ppstm (SCONST (l,s)) = "STRING CONST: \"" ^ s ^ "\""
     | ppstm (FUNDEC (t1,s,p,body)) =
         "FUNDEC " ^ s ^ " (" ^ ppt p ^ ") {\n" ^ 
         prettyPrint body ^ "}\n"

   and prettyPrint [] = ""
     | prettyPrint (h::t) = ppstm h ^ "\n" ^ prettyPrint t

end
