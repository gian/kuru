(******************************************************************************
* The Kuruc Compiler (http://www.kuru-lang.org)
* Copyright (c) 2010-2013, Gian Perrone
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
* 
* - Redistributions of source code must retain the above copyright notice, this
*   list of conditions and the following disclaimer.
*
* - Redistributions in binary form must reproduce the above copyright notice,
*   this list of conditions and the following disclaimer in the documentation
*   and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
* POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************
*  This file:
*    A typed Abstract Syntax Tree
******************************************************************************)

structure TypedAst =
struct
   structure S = Symbol

   type symbol = S.t
   type pos = Position.t 

   datatype ty =
      TySimple of symbol
    | TyApp of ty * ty (* Follows application order, i.e. int list is ("list","int") *)
    | TyVar of symbol
    | TyTuple of ty list
    | TyArrow of ty * ty
    | TyMeta of int
    | TySeq of ty list
    | TyUnion of ty * constructor list
   withtype constructor = symbol * ty option

   fun ppty (TySimple s) = S.toString s
     | ppty (TyApp (t1, t2)) = ppty t2 ^ " " ^ ppty t1
     | ppty (TyMeta i) = "?x" ^ Int.toString i
     | ppty (TyTuple l) = String.concatWith " * " (map ppty l)
     | ppty (TyArrow (t1, t2)) = ppty t1 ^ " -> " ^ ppty t2
     | ppty (TyVar s) = "'" ^ S.toString s
     | ppty (TySeq [x]) = ppty x
     | ppty (TySeq l) = "(" ^ String.concatWith "," (map ppty l) ^ ")"
     | ppty (TyUnion (t, c)) = String.concatWith " | "
         (map (fn (s, NONE) => S.toString s
               | (s, SOME a) => S.toString s ^ " of " ^ ppty a) c)

   type constructor_tag = int

   datatype ascription =
      Opaque
    | Transparent

   datatype literal =
      Int of IntInf.int
    | Real of string
    | Char of int
    | String of int vector
    | Unit

   (* A note on desugaring patterns:
    *
    * Wildcards (_) get desugared to a fresh IdentifierPattern
    * Infix patterns (h::t) get turned into constructor patterns.
    * List patterns ([1,2,3,4]) get turned into constructor patterns.
    * Nil (nil, []) gets turned into a constructor pattern.
    *)
   datatype pattern =
      LiteralPattern of literal * pos
    | TuplePattern of pattern list * ty * pos
    | ConstructorPattern of symbol * pattern option * ty * pos
    | IdentifierPattern of symbol * ty * pos
    | AsPattern of pattern * pattern * ty * pos
    | CurriedPattern of pattern list * ty * pos
    | AnnotationPattern of pattern * ty * pos
    | UnitPattern of pos

   datatype expression =
      LiteralExpr of literal * pos
    | IdentifierExpr of symbol * ty * pos  (* Long and short identifiers. *)
    | TupleExpr of expression list * ty * pos
    | IfExpr of expression * expression * expression * ty * pos
    | WhileExpr of expression * expression * ty * pos
    | FnExpr of clause list * ty * pos
    | CaseExpr of expression * clause list * ty * pos
    | ApplyExpr of expression * expression * ty * pos
    | RaiseExpr of expression * pos
    | HandleExpr of expression * clause list * ty * pos
    | SeqExpr of expression list * ty * pos
    | LetExpr of declaration list * expression * ty * pos
    | AnnotationExpr of expression * ty * pos
    | ConstructorExpr of symbol * expression option * ty * pos

   and declaration =
      ValDec of clause * pos
    | ValRecDec of clause * pos
    | FunDec of symbol * clause list * pos
    | MutualDec of declaration list * pos
    | OpenDec of symbol * pos
    | BindDec of symbol * declaration * pos
    | TypeDec of ty * ty * pos
    | ValDef of symbol * ty * pos
    | TypeDef of ty * pos
    | StructureMod of symbol * declaration option * declaration * pos
    | SignatureMod of symbol * declaration * pos
    | StructureAssignMod of symbol * symbol * pos
    | StructMod of declaration list * pos
    | SigMod of declaration list * pos
    | RefMod of symbol * pos
    | FunctorMod of symbol * functor_clause list * declaration * pos

   withtype clause = pattern * expression
        and functor_clause = declaration * declaration

   type program = declaration list

   fun indent 0 = ""
     | indent n = "   " ^ indent (n - 1)

   fun pplit (Int i) = IntInf.toString i
     | pplit (Real s) = s
     | pplit (Char i) = "#\"" ^ String.str (Char.chr i) ^ "\""
     | pplit (String s) = "\"" ^ (Vector.map Char.chr s) ^ "\""
     | pplit Unit = "()"

   fun ppexp ind (LiteralExpr (l,_)) = pplit l
     | ppexp ind (IdentifierExpr (symbol, _, _)) = S.toString symbol
     | ppexp ind (TupleExpr (exprs, _, _)) = 
         "(" ^ String.concatWith ", " (map (ppexp ind) exprs) ^ ")"
     | ppexp ind (IfExpr (e1, e2, e3, _, _)) =
         "if " ^ ppexp ind e1 ^ " then " ^
            ppexp ind e2 ^ " else " ^
            ppexp ind e3
     | ppexp ind (WhileExpr (e1, e2, _, _)) =
         "while " ^ ppexp ind e1 ^ " do " ^
            ppexp ind e2
     | ppexp ind (ApplyExpr (e1, e2, _, _)) =
         ppexp ind e1 ^ " " ^ ppexp ind e2
     | ppexp ind (LetExpr (decls, exp, _, _)) =
         "\n" ^ indent ind ^ "let\n" ^
            String.concatWith "\n" (map (ppdec (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "in\n" ^
         indent (ind + 1) ^ ppexp (ind + 1) exp ^ "\n" ^
         indent ind ^ "end"
     | ppexp ind (CaseExpr (exp, ((p,b)::clauses), _, _)) =
         "case " ^ ppexp ind exp ^ " of\n" ^
            indent (ind + 1) ^ "   " ^ pppat p ^ " => " ^
               ppexp (ind + 1) b ^ String.concat
                   (map (fn (p,b) => 
                     "\n" ^ indent (ind + 1) ^ "  | " ^
                        pppat p ^ " => " ^ ppexp (ind + 1) b) clauses)
     | ppexp ind (FnExpr (((p,b)::clauses), _, _)) =
         "fn " ^ pppat p ^ " => " ^ ppexp ind b
     | ppexp ind (SeqExpr (exprs, _, _)) = 
         "(" ^ String.concatWith "; " (map (ppexp ind) exprs) ^ ")"
     | ppexp ind (HandleExpr (e, (p,b)::clauses, _, _)) =
         ppexp ind e ^ " handle " ^ pppat p ^ " => " ^ ppexp ind b
     | ppexp ind (ConstructorExpr (symbol, NONE, _, _)) = S.toString symbol
     | ppexp ind (ConstructorExpr (symbol, SOME args, _, _)) = 
         "(" ^ S.toString symbol ^ " " ^ ppexp ind args ^ ")"
     | ppexp ind (AnnotationExpr (exp, ty, _)) =
         ppexp ind exp ^ " : " ^ ppty ty
     | ppexp ind (RaiseExpr (exp, _)) = "raise " ^ ppexp ind exp
     | ppexp ind p = 
         Error.bug ("TypedAst.ppexp", "Unknown or invalid AST", (0,""))

   and pppat (LiteralPattern (p, _)) = pplit p
     | pppat (IdentifierPattern (symbol, ty, pos)) = S.toString symbol
     | pppat (TuplePattern (pats, _, _)) = 
         "(" ^ String.concatWith "," (map pppat pats) ^ ")"
     | pppat (CurriedPattern (pats, _, _)) =
         String.concatWith " " (map pppat pats)
     | pppat (AnnotationPattern (pat, ty, pos)) =
         "(" ^ pppat pat ^ " : " ^ ppty ty ^ ")"
     | pppat (ConstructorPattern (symbol, NONE, _, _)) = S.toString symbol
     | pppat (ConstructorPattern (symbol, SOME pat, _, _)) = 
         "(" ^ S.toString symbol ^ " " ^ pppat pat ^ ")"
     | pppat (AsPattern (pat1, pat2, _, _)) = 
         pppat pat1 ^ " as " ^ pppat pat2
     | pppat _ = "(* pat *)"

   and ppdef ind _ = indent ind ^ "(* definition *)"

   and ppclauses ind symb ((pat, exp)::t) =
      "\n" ^ indent ind ^ "  | " ^
         S.toString symb ^ " " ^ pppat pat ^ " = " ^ ppexp ind exp ^
         ppclauses ind symb t
     | ppclauses ind symb [] = ""

   and ppdec ind (StructureMod (symbol, NONE, str, _)) =
         indent ind ^ "structure " ^ S.toString symbol ^ " =\n" ^
            ppdec ind str
     | ppdec ind (StructureMod (symbol, SOME sg, str, _)) =
         indent ind ^ "structure " ^ S.toString symbol ^ " :\n" ^
         ppdec (ind + 1) sg ^ 
         " =\n" ^
         ppdec (ind + 1) str
     | ppdec ind (SignatureMod (symbol, sg, _)) =
         indent ind ^ "signature " ^ S.toString symbol ^ " =\n" ^
            ppdec ind sg
     | ppdec ind (StructMod (decls, _)) =
         indent ind ^ "struct\n" ^
            String.concatWith "\n" (map (ppdec (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "end"
     | ppdec ind (StructureAssignMod (lhs, rhs, pos)) =
         indent ind ^ "structure " ^ S.toString lhs ^ " = " ^ S.toString rhs
     | ppdec ind (SigMod (decls, _)) =
         indent ind ^ "sig\n" ^
            String.concatWith "\n" (map (ppdef (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "end"
     | ppdec ind (RefMod (symbol, _)) = indent ind ^ S.toString symbol 
     | ppdec ind (FunctorMod (symbol, fclause, body, _)) = indent ind ^
         "functor " ^ S.toString symbol ^ " (...) =\n" ^
            ppdec ind body
     | ppdec ind (ValDec ((pat, expr), pos)) =
         indent ind ^ "val " ^ pppat pat ^ " = " ^ ppexp ind expr
     | ppdec ind (ValRecDec ((pat, expr), pos)) =
         indent ind ^ "val rec " ^ pppat pat ^ " = " ^ ppexp ind expr
     | ppdec ind (FunDec (symbol, (pat, expr)::t, pos)) =
         indent ind ^ "fun " ^ S.toString symbol ^ " " ^ pppat pat ^ " = " ^ 
            ppexp ind expr ^ ppclauses ind symbol t
     | ppdec ind (TypeDec (ty1, ty2, pos)) =
         indent ind ^ "type " ^ ppty ty1 ^ " = " ^ ppty ty2
     | ppdec ind (MutualDec (decls, pos)) = 
          String.concatWith 
             ("\n" ^ indent (ind) ^ "and ") 
                (map (ppdec ind) decls)

   fun prettyPrint [] = ""
     | prettyPrint (h::t) = ppdec 0 h ^ "\n\n" ^ prettyPrint t
end
