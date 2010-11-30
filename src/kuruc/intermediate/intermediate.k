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
    | Escapes
    | Custom of string
      (* TODO: Add other attributes here. *)

   fun addAttr l e = e :: l

   type metadata = attribute list

   datatype ir =
      N of ir_opr * ir list * T.ty * metadata * pos

   and ir_opr =
      INT of int
    | CHAR of char
    | STRING of string
    | REAL of real
    | BOOL of bool
    | UNIT
    | IDENT of S.symbol
    | EXTERN of S.symbol
    | ENV of int * S.symbol
    | SELF of S.symbol
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
    | BIND of S.symbol
    | MUTUAL
    | PWILD
    | NIL
    | CONS
    | EQ
    | PINFIX of S.symbol
    | IMPORT of S.symbol * T.ty * S.symbol
    | STUB
    | FUNDEC of S.symbol
    | ALIAS of S.symbol * S.symbol * T.ty
    | PROGRAM

   fun ppopr (INT i) = Int.toString i
     | ppopr (CHAR c) = "#\"" ^ Char.toString c ^ "\""
     | ppopr (STRING s) = "\"" ^ String.toCString s ^ "\""
     | ppopr (REAL r) = Real.toString r
     | ppopr (BOOL true) = "true"
     | ppopr (BOOL false) = "false"
     | ppopr UNIT = "()"
     | ppopr (IDENT s) = S.name s
     | ppopr (EXTERN s) = "extern " ^ S.name s
     | ppopr (ENV (i,s)) = "*(env+1+" ^ Int.toString i ^ ", "^ S.name s ^ ")"
     | ppopr (SELF s) = "SELF " ^ S.name s
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
     | ppopr (BIND s) = "BIND " ^ S.name s
     | ppopr MUTUAL = "MUTUAL"
     | ppopr PWILD = "_"
     | ppopr (PINFIX s) = "PINFIX " ^ S.name s
     | ppopr (IMPORT (s1,t,s2)) = "IMPORT " ^ S.name s1 ^ " as " ^ S.name s2
     | ppopr NIL = "NIL"
     | ppopr CONS = "CONS"
     | ppopr EQ = "EQ"
     | ppopr STUB = "STUB"
     | ppopr (FUNDEC s) = "FUNDEC " ^ S.name s
     | ppopr (ALIAS (k,s,t)) = "ALIAS " ^ S.name k ^ " = " ^ S.name s
     | ppopr PROGRAM = "PROGRAM"

   fun ind 0 = ""
     | ind n = "   " ^ ind (n-1)

   fun pp i (N (opr,[],t,m,p)) = (ind i) ^ ppopr opr ^ " : " ^ T.prettyPrint t
     | pp i (N (opr,l,t,m,p)) = (ind i) ^  
         ppopr opr ^ " : " ^ T.prettyPrint t ^ " [\n" ^ 
            String.concatWith ",\n" (map (pp (i+1)) l) ^ "\n" ^ (ind i) ^ "]\n"
   
   val prettyPrint = pp 0

   fun mapChildren f (N (_,l,_,_,_)) = map f l

   fun mapType f (N (a,b,t,c,d)) = 
         N (a, map (mapType f) b, f t, c, d)

   fun getType (N (_,_,t,_,_)) = t

   fun size (N (_,[],_,_,_)) = 1
     | size (N (_,l,_,_,_)) = List.foldl (fn (c,s) => s + size c) 1 l

end

