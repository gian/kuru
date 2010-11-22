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
*    Intermediate representation.
*    This IR has these properties:
*      Patterns exist only in case expressions.
*      Everything is monomorphic.
*      Functions may still be higher-order
*      All names are unique.
******************************************************************************)
structure Intermediate =
struct
   structure S = Symbol
   structure T = Type

   type pos = ParseTree.pos

   datatype attribute =
      Associative
    | Commutative
    | Transitive
    | Reflexive
    | Pure
    | Custom of string
      (* TODO: Add other attributes here. *)

   type metadata = attribute list

   datatype ir =
      N of ir_opr * ir list * T.ty * metadata * pos

   and ir_opr =
      INT of int
    | STRING of string
    | REAL of real
    | BOOL of bool
    | UNIT
    | IDENT of S.symbol
    | TUPLE
    | IF
    | WHILE
    | FN
    | CASE
    | MATCH
    | APP
    | RAISE
    | HANDLE
    | SEQ
    | LET
    | CON of int
    | CLOSURE
    | BIND
    | MUTUAL
    | PCONST
    | PTUPLE
    | PWILD
    | PCONSTR of int
    | PINFIX of S.symbol
    | IMPORT of S.symbol * T.ty * S.symbol
    | PROGRAM

   fun ppopr (INT i) = Int.toString i
     | ppopr (STRING s) = "\"" ^ String.toCString s ^ "\""
     | ppopr (REAL r) = Real.toString r
     | ppopr (BOOL true) = "true"
     | ppopr (BOOL false) = "false"
     | ppopr UNIT = "()"
     | ppopr (IDENT s) = S.name s
     | ppopr TUPLE = "TUPLE"
     | ppopr IF = "IF"
     | ppopr WHILE = "WHILE"
     | ppopr FN = "FN"
     | ppopr CASE = "CASE"
     | ppopr MATCH = "MATCH"
     | ppopr APP = "APP"
     | ppopr RAISE = "RAISE"
     | ppopr HANDLE = "HANDLE"
     | ppopr SEQ = "SEQ"
     | ppopr LET = "LET"
     | ppopr (CON i) = "CON " ^ Int.toString i
     | ppopr CLOSURE = "CLOSURE"
     | ppopr BIND = "BIND"
     | ppopr MUTUAL = "MUTUAL"
     | ppopr PCONST = "PCONST"
     | ppopr PTUPLE = "PTUPLE"
     | ppopr PWILD = "_"
     | ppopr (PCONSTR i) = "PCONSTR " ^ Int.toString i
     | ppopr (PINFIX s) = "PINFIX " ^ S.name s
     | ppopr (IMPORT (s1,t,s2)) = "IMPORT " ^ S.name s1 ^ " as " ^ S.name s2
     | ppopr PROGRAM = "PROGRAM"

   fun ind 0 = ""
     | ind n = "   " ^ ind (n-1)

   fun pp i (N (opr,[],t,m,p)) = (ind i) ^ ppopr opr ^ " : " ^ T.prettyPrint t
     | pp i (N (opr,l,t,m,p)) = (ind i) ^  
         ppopr opr ^ " : " ^ T.prettyPrint t ^ " [\n" ^ 
            String.concatWith ",\n" (map (pp (i+1)) l) ^ "\n" ^ (ind i) ^ "]\n"
   
   val prettyPrint = pp 0

end

