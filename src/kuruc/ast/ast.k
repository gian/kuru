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
*    An untyped Abstract Syntax Tree.
*
*    In reality, this is just a typed AST with a different pretty printer.
******************************************************************************)

structure Ast =
struct
   open TypedAst

   fun indent 0 = ""
     | indent n = "   " ^ indent (n - 1)

   fun ppexp ind (LiteralExpr (l,_)) = pplit l
     | ppexp ind (IdentifierExpr (symbol, _, _)) = S.toString symbol
     | ppexp ind (TupleExpr (exprs, _, _)) = 
         "(" ^ String.concatWith ", " (map (ppexp ind) exprs) ^ ")"
     | ppexp ind (TupleExtractExpr (i, expr, _, _)) = 
         "#" ^ Int.toString i ^ " " ^ ppexp ind expr
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
     | ppexp ind (Struct (decls, _, _)) =
         indent ind ^ "struct\n" ^
            String.concatWith "\n" (map (ppdec (ind + 1)) decls) ^ "\n" ^
         indent ind ^ "end"
     | ppexp ind p = 
         Error.bug ("Ast.ppexp", "Unknown or invalid AST", (0,""))

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


   fun prettyPrint [] = ""
     | prettyPrint (h::t) = ppdec 0 h ^ "\n\n" ^ prettyPrint t
end