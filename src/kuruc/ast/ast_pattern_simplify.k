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
*    Converts patterns to "simple" patterns.
*
*    e.g., val (a,b) = E
*    is converted to
*    local
*     val tmp = E
*    in
*     val a = #1 tmp
*     val b = #2 tmp
*    end
*
*    fn (x, y) => E
*    is converted to
*    fn tmp => case (x, y) => E
*
*    val (a : int) = E
*
*    is converted to
*    val a = E : int
*
*    fun f (x, y, z) = E
*      | f (a, b, c) = E'
*
*    is converted to:
*
*    fun f tmp =
*     case tmp of
*        (x, y, z) => E
*      | (a, b, c) => E'
*
*    fun f a b c = E
*      | f x y z = E'
*
*    is converted to:
*
*    fun f tmp1 = fn tmp2 => fn tmp3 => 
*     case (tmp1, tmp2, tmp3) of
*        (a, b, c) => E
*      | (x, y, z) => E'
*
*   
*    The goal is that all that remains in declarations is patterns of the form:
*
*    IdentifierPattern (_, _, _) or LiteralPattern (Unit, _) or
*    ConstructorPattern (_, _, _) or AsPattern (
*
******************************************************************************)

structure AstPatternSimplify =
struct
   structure S = Symbol
   structure A = Ast
   structure P = ParseTree

   val freshIdx = ref 0

   fun freshName pos =
      (S.fromString ("_tmp" ^ Int.toString (!freshIdx)) pos) before
         (freshIdx := 1 + !freshIdx)

   fun freshTy () =
      (A.TyMeta (!freshIdx)) before
         (freshIdx := 1 + !freshIdx)

   fun isSimple (A.IdentifierPattern _) = true
     | isSimple (A.LiteralPattern (A.Unit, _)) = true
     | isSimple (A.ConstructorPattern (_, NONE, _, _)) = true
     | isSimple (A.ConstructorPattern (_, SOME p, _, _)) = isSimple p
     | isSimple e = 
      let
         val _ = Debug.print Debug.debug 
            ("isSimple (false): " ^ A.pppat e ^ "\n")
      in
         false
      end

   (* Transform single patterns into groups of declarations and
   * a (pattern, expression) tuple. *)
   fun simplifyPat (pat as A.IdentifierPattern (s, t, p)) exp = (pat, exp, [])
     | simplifyPat (pat as A.LiteralPattern(A.Unit, _)) exp = (pat, exp, [])
     | simplifyPat (pat as A.AsPattern (p1, p2, t, pos)) exp = 
      let
         val decl = A.ValDec ((p1, exp), pos)
      in
         (p2, exp, [decl])
      end
     | simplifyPat (A.AnnotationPattern (pat, ty, p)) exp = 
      let
         val (pat', exp', decls) = simplifyPat pat exp
      in
         (pat', A.AnnotationExpr (exp', ty, p), decls)
      end
     | simplifyPat (A.CurriedPattern ((ph::pt), ty, pos)) exp =
         Error.fail ("Invalid pattern", pos)
     | simplifyPat (A.TuplePattern (pats, ty, pos)) exp =
      let
         val tmp = freshName pos
         val pat' = A.IdentifierPattern (tmp, freshTy (), pos)
         val exp' = A.IdentifierExpr (tmp, freshTy (), pos)
         val offset = ref 1
         val _ = Debug.print Debug.debug ("simplifyPat for exp: " ^ A.ppexp 0 exp ^ "\n")
         val decls =
            List.foldl (fn (pat, decls) =>
               let
                  val (p', e', d') = simplifyPatFully (
                     pat,
                     A.TupleExtractExpr (!offset, exp', freshTy(), pos),
                     []) []

                  val _ = offset := 1 + !offset
               in
                  decls @ [(A.ValDec ((p', e'), pos))] @ d'
               end) [] pats
      in
         (pat', exp, decls)
      end
    | simplifyPat (A.ConstructorPattern (symb, SOME pat, _, pos)) exp =
      let
         (* This is only used for patterns of the form:
         *
         * val (Constructor (P)) = E,
         *
         * Which we turn into:
         *
         * val p = case E of (Constructor tmp) => tmp
         *)

         val tmp = freshName pos
         
         val caseExpr = A.CaseExpr (exp,
            [(A.ConstructorPattern (
                  symb, 
                  SOME (A.IdentifierPattern (tmp, freshTy(), pos)),
                  freshTy (),
                  pos
               ),
               A.IdentifierExpr (tmp, freshTy(), pos)
            )],
            freshTy (),
            pos)
      in
         simplifyPatFully (pat, caseExpr, []) []         
      end
    | simplifyPat pat exp = (pat, simplifyExpr exp, [])

   and simplifyPatFully (pat, exp, decls) declsSum =
      if isSimple pat then (pat, exp, declsSum @ decls) else
         simplifyPatFully (simplifyPat pat exp) (declsSum @ decls)

   and checkClauses [] = ()
     | checkClauses clauses =
      let
         fun chk (A.CurriedPattern (c, _, p),_) (A.CurriedPattern (c', _, p'),_) =
            if length c = length c' then () else 
               Error.fail ("Clauses have different numbers of arguments", p')
           | chk pat (A.CurriedPattern (_,_,p'),_) =
               Error.fail ("Clauses have different numbers of arguments", p')
           | chk _ _ = Error.bug ("AstPatternSimplify.checkClauses", "chk",
                           (0, "unknown"))
      in
         (map chk clauses; ())
      end

   and simplifyClauses pos [(ident as A.IdentifierPattern _, exp)] = 
         [(ident, simplifyExpr exp)]
     | simplifyClauses pos [(pat as A.LiteralPattern (A.Unit, _), exp)] = 
         [(pat, simplifyExpr exp)]
     | simplifyClauses pos (clauses as ((A.CurriedPattern (pats, _, pos'),exp)::t)) =
      let
         val _ = checkClauses clauses

         val tmps = map (fn _ => freshName pos') pats
         val (pat' :: fnPats) = map 
            (fn t => A.IdentifierPattern (t, freshTy (), pos)) tmps
         val caseExp = 
            A.CaseExpr (
               A.TupleExpr (
                  map (fn t => A.IdentifierExpr (t, freshTy(), pos)) tmps,
                  freshTy (),
                  pos),
               map (fn (A.CurriedPattern (pats, _, pos'), exp) =>
                  (A.TuplePattern (pats, freshTy (), pos'), simplifyExpr exp))
                     clauses,
               freshTy (),
               pos)

         val body = expandPartialApplication pos fnPats caseExp
      in
         [(pat', body)] 
      end
     | simplifyClauses pos clauses =
   let
      val _ = checkClauses clauses

      val tmp = freshName pos
      val pat' = A.IdentifierPattern (tmp, freshTy (), pos)
      val exp' = A.IdentifierExpr (tmp, freshTy (), pos)
      val clauses' = 
         map (fn (pat, exp) => (pat, simplifyExpr exp)) clauses
   in
      [(pat', A.CaseExpr (exp', clauses', freshTy(), pos))]
   end

   and expandPartialApplication pos pats exp =
      List.foldl (fn (pat, fns) =>
         A.FnExpr ([(pat, fns)], freshTy(), pos))
            exp pats
   
   and simplifyExpr' (A.TupleExpr (exps, ty, pos)) =
         A.TupleExpr (map simplifyExpr exps, ty, pos)
 (*    | simplifyExpr' (A.TupleExtractExpr (i, exp', ty, pos)) =
         A.TupleExtractExpr (i, simplifyExpr exp', ty, pos) *)
     | simplifyExpr' (A.IfExpr (e1, e2, e3, ty, pos)) =
         A.IfExpr (simplifyExpr e1, simplifyExpr e2, simplifyExpr e3, ty, pos)
     | simplifyExpr' (A.WhileExpr (e1, e2, ty, pos)) =
         A.WhileExpr (simplifyExpr e1, simplifyExpr e2, ty, pos) 
     | simplifyExpr' (A.FnExpr (clauses, ty, pos)) =
         A.FnExpr (simplifyClauses pos clauses, ty, pos)
     | simplifyExpr' (A.CaseExpr (expression, clauses, ty, pos)) =
         A.CaseExpr (simplifyExpr expression,
            map (fn (p,expr) => (p, simplifyExpr expr)) clauses, ty, pos)
     | simplifyExpr' (A.ApplyExpr (e1, e2, ty, pos)) =
         A.ApplyExpr (simplifyExpr e1, simplifyExpr e2, ty, pos)
     | simplifyExpr' (A.RaiseExpr (exp, pos)) =
         A.RaiseExpr (simplifyExpr exp, pos)
     | simplifyExpr' (A.HandleExpr (exp, clauses, ty, pos)) =
         A.HandleExpr (simplifyExpr exp,
            simplifyClauses pos clauses, ty, pos)
     | simplifyExpr' (A.SeqExpr (exps, ty, pos)) =
         A.SeqExpr (map simplifyExpr exps, ty, pos)
     | simplifyExpr' (A.LetExpr (decls, exp, ty, pos)) =
         A.LetExpr (simplifyDecls decls, simplifyExpr exp, ty, pos)
     | simplifyExpr' (A.AnnotationExpr (exp, ty, pos)) =
         A.AnnotationExpr (simplifyExpr exp, ty, pos)
     | simplifyExpr' (A.ConstructorExpr (symb, SOME exp, ty, pos)) =
         A.ConstructorExpr (symb, SOME (simplifyExpr exp), ty, pos)
     | simplifyExpr' (A.Struct (decls, ty, pos)) =
         A.Struct (simplifyDecls decls, ty, pos) 
     | simplifyExpr' exp = exp

   and simplifyExpr expr =
      let
         val _ = Debug.print Debug.debug ("simplifyExpr: " ^ A.ppexp 0 expr ^ "\n")
      in
         simplifyExpr' expr
      end

   and simplifyDecls ((A.ValDec ((pat, exp), pos)) :: t) =
      let
         val (pat', exp', decls) = simplifyPat pat (simplifyExpr exp)
      in
         A.ValDec ((pat', exp'), pos) :: decls @ simplifyDecls t
      end
     | simplifyDecls ((A.ValRecDec ((pat, exp), pos)) :: t) =
      let
         val (pat', exp', decls) = simplifyPat pat (simplifyExpr exp)
      in
         A.ValRecDec ((pat', exp'), pos) :: decls @ simplifyDecls t
      end
     | simplifyDecls ((A.StructureMod (s, body, pos))::t) =
         A.StructureMod (s, simplifyExpr body, pos) :: simplifyDecls t
     | simplifyDecls ((A.FunDec (sym, clauses, pos))::t) =
      let
         (* Get rid of FunDecs entirely here. *)
         val (pat, exp) = 
            case simplifyClauses pos clauses of
               [pe] => pe
             | _ => 
               Error.bug ("AstPatternSimplify", "simplifyClauses invalid", pos)
      in
         A.ValRecDec (
            (A.IdentifierPattern (sym, freshTy (), pos), 
             A.FnExpr ([(pat, exp)], freshTy (), pos)),
            pos
         )
      end :: simplifyDecls t
     | simplifyDecls ((A.MutualDec (decls, pos))::t) =
         A.MutualDec (simplifyDecls decls, pos) :: simplifyDecls t
     | simplifyDecls (h::t) = h :: simplifyDecls t
     | simplifyDecls [] = []
   
   fun simplifyPatterns program =
      simplifyDecls program
end

