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
*
*    is converted to:
*
*    fun f a = fn b => fn c => E
*
*
*    The goal is that all that remains in declarations is patterns of the form:
*
*    IdentifierPattern (_, _, _)
*
******************************************************************************)

structure AstPatternSimplify =
struct
   structure S = Symbol
   structure A = TypedAst
   structure P = ParseTree

   val freshIdx = ref 0

   fun freshName pos =
      (S.fromString ("_tmp" ^ Int.toString (!freshIdx)) pos) before
         (freshIdx := 1 + !freshIdx)

   fun freshTy () =
      (A.TyMeta (!freshIdx)) before
         (freshIdx := 1 + !freshIdx)

   fun isSimple (A.IdentifierPattern _) = true
     | isSimple (A.UnitPattern _) = true
     | isSimple _ = false

   (* Transform single patterns into groups of declarations and
   * a (pattern, expression) tuple. *)
   fun simplifyPat (pat as A.IdentifierPattern (s, t, p)) exp = (pat, exp, [])
     | simplifyPat (pat as A.UnitPattern _) exp = (pat, exp, [])
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
    | simplifyPat pat exp = (pat, exp, [])

   and simplifyPatFully (pat, exp, decls) declsSum =
      if isSimple pat then (pat, exp, declsSum @ decls) else
         simplifyPatFully (simplifyPat pat exp) (declsSum @ decls)

   fun simplifyDecls ((A.ValDec ((pat, exp), pos)) :: t) =
      let
         val (pat', exp', decls) = simplifyPat pat exp
      in
         A.ValDec ((pat', exp'), pos) :: decls @ simplifyDecls t
      end
     | simplifyDecls ((A.ValRecDec ((pat, exp), pos)) :: t) =
      let
         val (pat', exp', decls) = simplifyPat pat exp
      in
         A.ValRecDec ((pat', exp'), pos) :: decls @ simplifyDecls t
      end
     | simplifyDecls ((A.StructureMod (s, d, decl, pos))::t) =
         A.StructureMod (s, d, hd (simplifyDecls [decl]), pos) :: simplifyDecls t
     | simplifyDecls ((A.StructMod (decls, pos))::t) =
         A.StructMod (simplifyDecls decls, pos) :: simplifyDecls t
     | simplifyDecls (h::t) = h :: simplifyDecls t
     | simplifyDecls [] = []
   
   fun simplifyPatterns program =
      simplifyDecls program
end

