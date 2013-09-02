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
    | TyVar of int
    | TyTuple of ty list
    | TyArrow of ty * ty
    | TyMeta of int
    | TySeq of ty list
    | TyUnion of ty * constructor list
    | TyLam of ty * ty 
    | TySig of unit
   withtype constructor = symbol * ty option

   fun substInTy (TyApp (t1, t2)) t t' =
         TyApp (substInTy t1 t t', substInTy t2 t t')
     | substInTy (TyTuple ts) t t' = 
         TyTuple (map (fn tx => substInTy tx t t') ts)
     | substInTy (TyArrow (t1, t2)) t t' =
         TyArrow (substInTy t1 t t', substInTy t2 t t')
     | substInTy (TySeq ts) t t' = 
         TySeq (map (fn tx => substInTy tx t t') ts)
     | substInTy (TyUnion (ty, constrs)) t t' =
         TyUnion (
            substInTy ty t t',
            map (fn (s, SOME tx) => (s, SOME (substInTy tx t t'))
                  | (s, NONE) => (s, NONE)) constrs
         )
     | substInTy (TyLam (t1, t2)) t t' = 
         Error.bug ("TypedAst.substInTy", "TyLam encountered", (0, "__fake__"))
     | substInTy (ot as (TySimple s)) (TySimple t) t' = 
         if S.equals (s, t) then t' else ot
     | substInTy (ot as (TyVar i)) (TyVar j) t' = 
         if i = j then t' else ot
     | substInTy (ot as (TyMeta i)) (TyMeta j) t' =
         if i = j then t' else ot
     | substInTy ot _ _ = ot

   val nextTyIndex = ref 0
   fun freshTy () =
         TyMeta (nextTyIndex := 1 + !nextTyIndex; !nextTyIndex)
   fun resetFreshTy () = nextTyIndex := 0

   fun ppty (TySimple s) = S.toString s
     | ppty (TyApp (t1, t2)) = "[" ^ ppty t2 ^ " " ^ ppty t1 ^ "]"
     | ppty (TyMeta i) = "?x" ^ Int.toString i
     | ppty (TyTuple l) = "(" ^ String.concatWith " * " (map ppty l) ^ ")"
     | ppty (TyArrow (t1, t2)) = ppty t1 ^ " -> " ^ ppty t2
     | ppty (TyVar i) = "'" ^ (String.str (Char.chr (i + Char.ord(#"a"))))
     | ppty (TySeq [x]) = "(" ^ ppty x ^ ")"
     | ppty (TySeq l) = "(" ^ String.concatWith "," (map ppty l) ^ ")"
     | ppty (TyUnion (t, c)) = "{" ^ String.concatWith " | "
         (map (fn (s, NONE) => S.toString s
               | (s, SOME a) => S.toString s ^ " of " ^ ppty a) c) ^ "}"
     | ppty (TyLam (t1, t2)) = "\206\187" ^ ppty t1 ^ "." ^ ppty t2
     | ppty (TySig _) = "(* SIG TYPE TODO *)"

   type constructor_tag = int

   datatype ascription =
      Opaque
    | Transparent

   datatype literal =
      Int of IntInf.int
    | Real of string
    | Char of int
    | String of int vector
    | Bool of bool
    | Word of string
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
    | ImplicitPattern of (pattern * ty) list * pos

   datatype expression =
      LiteralExpr of literal * pos
    | IdentifierExpr of symbol * ty * pos  (* Long and short identifiers. *)
    | TupleExpr of expression list * ty * pos
    | TupleExtractExpr of int * expression * ty * pos
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
    | Struct of declaration list * ty * pos

   and declaration =
      ValDec of clause * pos
    | ValRecDec of clause * pos
    | FunDec of symbol * clause list * pos
    | MutualDec of declaration list * pos
    | LocalDec of declaration list * declaration list * pos
    | OpenDec of symbol * pos
    | BindDec of symbol * declaration * pos
    | TypeDec of ty * ty * pos
    | ValDef of symbol * ty * pos
    | TypeDef of ty * pos
    | StructureMod of symbol * expression * pos
    | SignatureMod of symbol * declaration * pos
    | StructureAssignMod of symbol * symbol * pos
    | SigMod of declaration list * pos
    | RefMod of symbol * pos
    | FunctorMod of symbol * functor_clause list * declaration * pos

   withtype clause = pattern * expression
        and functor_clause = declaration * declaration

   type program = declaration list

   fun getPos _ = (0, "fake")

   fun indent 0 = ""
     | indent n = "   " ^ indent (n - 1)

   fun pplit (Int i) = IntInf.toString i
     | pplit (Real s) = s
     | pplit (Char i) = "#\"" ^ String.str (Char.chr i) ^ "\""
     | pplit (String s) = "\"" ^ (Vector.map Char.chr s) ^ "\""
     | pplit (Bool true) = "true"
     | pplit (Bool false) = "false"
     | pplit (Word s) = "0w" ^ s
     | pplit Unit = "()"

   fun ppexp ind (LiteralExpr (l,_)) = pplit l
     | ppexp ind (IdentifierExpr (symbol, ty, _)) = S.toString symbol ^ " : " ^
         ppty ty
     | ppexp ind (TupleExpr (exprs, ty, _)) = 
         "(" ^ String.concatWith ", " (map (ppexp ind) exprs) ^ ") : " ^
            ppty ty
     | ppexp ind (TupleExtractExpr (i, expr, _, _)) = 
         "#" ^ Int.toString i ^ " " ^ ppexp ind expr
     | ppexp ind (IfExpr (e1, e2, e3, ty, _)) =
         "if " ^ ppexp ind e1 ^ " then " ^
            ppexp ind e2 ^ " else " ^
            ppexp ind e3 ^ " : " ^ ppty ty
     | ppexp ind (WhileExpr (e1, e2, _, _)) =
         "while " ^ ppexp ind e1 ^ " do " ^
            ppexp ind e2
     | ppexp ind (ApplyExpr (e1, e2, ty, _)) =
         ppexp ind e1 ^ " (" ^ ppexp ind e2 ^ ") : " ^ ppty ty
     | ppexp ind (LetExpr (decls, exp, ty, _)) =
         "\n" ^ indent ind ^ "let\n" ^
            String.concatWith "\n" (map (ppdec (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "in\n" ^
         indent (ind + 1) ^ ppexp (ind + 1) exp ^ "\n" ^
         indent ind ^ "end : " ^ ppty ty
     | ppexp ind (CaseExpr (exp, ((p,b)::clauses), _, _)) =
         "case " ^ ppexp ind exp ^ " of\n" ^
            indent (ind + 1) ^ "   " ^ pppat p ^ " => " ^
               ppexp (ind + 1) b ^ String.concat
                   (map (fn (p,b) => 
                     "\n" ^ indent (ind + 1) ^ "  | " ^
                        pppat p ^ " => " ^ ppexp (ind + 1) b) clauses)
     | ppexp ind (FnExpr (((p,b)::clauses), t, _)) =
         "fn " ^ pppat p ^ " => " ^ ppexp ind b ^ " : " ^ ppty t
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
     | ppexp ind (Struct (decls, _, _)) =
         indent ind ^ "struct\n" ^
            String.concatWith "\n" (map (ppdec (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "end"
     | ppexp ind p = 
         Error.bug ("Ast.ppexp", "Unknown or invalid AST", (0,""))

   and pppat (LiteralPattern (p, _)) = pplit p
     | pppat (IdentifierPattern (symbol, ty, pos)) = "(" ^ S.toString symbol ^ " : " ^
     ppty ty ^ ")"
     | pppat (TuplePattern (pats, ty, _)) = 
         "(" ^ String.concatWith "," (map pppat pats) ^ ") : " ^ ppty ty
     | pppat (CurriedPattern (pats, _, _)) =
         String.concatWith " " (map pppat pats)
     | pppat (AnnotationPattern (pat, ty, pos)) =
         "(" ^ pppat pat ^ " : " ^ ppty ty ^ ")"
     | pppat (ConstructorPattern (symbol, NONE, t, _)) = S.toString symbol ^ " : " ^ ppty t
     | pppat (ConstructorPattern (symbol, SOME pat, t, _)) = 
         "(" ^ S.toString symbol ^ " " ^ pppat pat ^ " : " ^ ppty t ^ ")"
     | pppat (AsPattern (pat1, pat2, _, _)) = 
         pppat pat1 ^ " as " ^ pppat pat2
     | pppat (ImplicitPattern ([(pat,ty)], pos)) =
         "{" ^ pppat pat ^ " : " ^ ppty ty ^ "}"
     | pppat (ImplicitPattern (bindings, pos)) =
         "{" ^ String.concatWith ", "
            (map (fn (pat, ty) =>
               pppat pat ^ " : " ^ ppty ty) bindings) ^ "}"
     | pppat _ = "(* pat *)"

   and ppdef ind _ = indent ind ^ "(* definition *)"

   and ppclauses ind symb ((pat, exp)::t) =
      "\n" ^ indent ind ^ "  | " ^
         S.toString symb ^ " " ^ pppat pat ^ " = " ^ ppexp ind exp ^
         ppclauses ind symb t
     | ppclauses ind symb [] = ""

   and ppdec ind (StructureMod (symbol, str, _)) =
         indent ind ^ "structure " ^ S.toString symbol ^ " =\n" ^
            ppexp ind str
     | ppdec ind (SignatureMod (symbol, sg, _)) =
         indent ind ^ "signature " ^ S.toString symbol ^ " =\n" ^
            ppdec ind sg
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
     | ppdec ind (LocalDec (decls1, decls2, pos)) =
         indent ind ^ "local\n" ^
         String.concatWith "\n" (map (ppdec (ind+1)) decls1) ^ "\n" ^
         indent ind ^ "in\n" ^
         String.concatWith "\n" (map (ppdec (ind+1)) decls2) ^ "\n" ^
         indent ind ^ "end"
     | ppdec ind (ValDef (s, t, _)) = 
         indent ind ^ "val " ^ S.toString s ^ " : " ^ ppty t
     | ppdec ind (TypeDef (t, pos)) =
         indent ind ^ "type " ^ ppty t
     | ppdec ind (OpenDec (s, _)) = 
         indent ind ^ "open " ^ S.toString s


   fun prettyPrint [] = ""
     | prettyPrint (h::t) = ppdec 0 h ^ "\n\n" ^ prettyPrint t

   fun substInPattern (TuplePattern (pats, ty, pos)) t t' =
         TuplePattern (
            map (fn p => substInPattern p t t') pats,
            substInTy ty t t',
            pos
         )
     | substInPattern (ConstructorPattern (symb, NONE, ty, pos)) t t' =
         ConstructorPattern (
            symb,
            NONE,
            substInTy ty t t',
            pos
         )
     | substInPattern (ConstructorPattern (symb, SOME pat, ty, pos)) t t' =
         ConstructorPattern (
            symb,
            SOME (substInPattern pat t t'),
            substInTy ty t t',
            pos
         )

     | substInPattern (IdentifierPattern (symb, ty, pos)) t t' =
         IdentifierPattern (
            symb,
            substInTy ty t t',
            pos
         )
     | substInPattern (AsPattern (p1, p2, ty, pos)) t t' =
         AsPattern (
            substInPattern p1 t t',
            substInPattern p2 t t',
            substInTy ty t t',
            pos
         )
     | substInPattern (AnnotationPattern (pat,ty,pos)) t t' =
         AnnotationPattern (substInPattern pat t t', substInTy ty t t', pos)
     | substInPattern (ImplicitPattern _) _ _ = raise Fail ("substInPattern ImplicitPattern not impl")
     | substInPattern pat _ _ = pat

   fun substInExpr (IdentifierExpr (symb, ty, pos)) t t' =
         IdentifierExpr (
            symb,
            substInTy ty t t',
            pos
         )
     | substInExpr (TupleExpr (exps, ty, pos)) t t' =
         TupleExpr (
            map (fn e => substInExpr e t t') exps,
            substInTy ty t t',
            pos
         )
     | substInExpr (TupleExtractExpr (i,exp,ty,pos)) t t' =
         TupleExtractExpr (
            i,
            substInExpr exp t t',
            substInTy ty t t',
            pos
         )
     | substInExpr (IfExpr (e1, e2, e3, ty, pos)) t t' =
         IfExpr (
            substInExpr e1 t t',
            substInExpr e2 t t',
            substInExpr e3 t t',
            substInTy ty t t',
            pos
         )
     | substInExpr (WhileExpr (e1, e2, ty, pos)) t t' =
         WhileExpr (
            substInExpr e1 t t',
            substInExpr e2 t t',
            substInTy ty t t',
            pos
         )
     | substInExpr (FnExpr (clauses, ty, pos)) t t' =
         FnExpr (
            map (fn (p,e) => (
               substInPattern p t t',
               substInExpr e t t'
            )) clauses,
            substInTy ty t t',
            pos
         )
     | substInExpr (CaseExpr (exp, clauses, ty, pos)) t t' =
         CaseExpr (
            substInExpr exp t t',
            map (fn (p,e) => (
               substInPattern p t t',
               substInExpr e t t'
            )) clauses,
            substInTy ty t t',
            pos
         )
     | substInExpr (ApplyExpr (e1, e2, ty, pos)) t t' =
         ApplyExpr (
            substInExpr e1 t t',
            substInExpr e2 t t',
            substInTy ty t t',
            pos
         )

     | substInExpr (RaiseExpr (exp, pos)) t t' =
         RaiseExpr (
            substInExpr exp t t',
            pos
         )  
     | substInExpr (HandleExpr (exp, clauses, ty, pos)) t t' = 
         HandleExpr (
            substInExpr exp t t',
            map (fn (p,e) => (
               substInPattern p t t',
               substInExpr e t t'
            )) clauses,
            substInTy ty t t',
            pos
         )
     | substInExpr (SeqExpr (exps, ty, pos)) t t' =
         SeqExpr (
            map (fn e => substInExpr e t t') exps,
            substInTy ty t t',
            pos
         )
     | substInExpr (LetExpr (decls, exp, ty, pos)) t t' =
         LetExpr (
            map (fn d => substInDecl d t t') decls,
            substInExpr exp t t',
            substInTy ty t t',
            pos
         )
     | substInExpr (AnnotationExpr (exp, ty, pos)) t t' =
         AnnotationExpr (
            substInExpr exp t t',
            substInTy ty t t',
            pos
         )
     | substInExpr (ConstructorExpr (symb, NONE, ty, pos)) t t' =
         ConstructorExpr (
            symb,
            NONE,
            substInTy ty t t',
            pos
         )
     | substInExpr (ConstructorExpr (symb, SOME exp, ty, pos)) t t' =
         ConstructorExpr (
            symb,
            SOME (substInExpr exp t t'),
            substInTy ty t t',
            pos
         )
     | substInExpr (Struct (decls, ty, pos)) t t' =
         Struct (
            map (fn d => substInDecl d t t') decls,
            substInTy ty t t',
            pos
         )
     | substInExpr e _ _ = e

   and substInDecl (ValDec ((pat,exp), pos)) t t' =
         ValDec (
            (
               substInPattern pat t t',
               substInExpr exp t t'
            ),
            pos
         )
     | substInDecl (ValRecDec ((pat,exp), pos)) t t' =
         ValRecDec (
            (
               substInPattern pat t t',
               substInExpr exp t t'
            ),
            pos
         )
     | substInDecl (MutualDec (decls, pos)) t t' =
         MutualDec (
            map (fn d => substInDecl d t t') decls,
            pos
         )
     | substInDecl (LocalDec (decls1, decls2, pos)) t t' =
         LocalDec (
            map (fn d => substInDecl d t t') decls1,
            map (fn d => substInDecl d t t') decls2,
            pos
         )
     | substInDecl (StructureMod (symb, exp, pos)) t t' =
         StructureMod (
            symb,
            substInExpr exp t t',
            pos
         )
     | substInDecl d _ _ = d

end
