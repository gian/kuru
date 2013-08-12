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
*    Infers types for an untyped AST.
*
*    This is based substantially upon the type checker for substandard ML
*    (https://github.com/finrod/ssml/), by Filip Sieczkowski.
******************************************************************************)

structure TypeInference =
struct
   structure S = Symbol
   structure A = Ast
   structure T = TypedAst
   structure P = ParseTree

   datatype cdecl   = VDec of A.ty
   datatype closure = CPair of cdecl * (S.t * closure) list
   type environment = (S.t * closure) list

   val tyvarCounter = ref 0

   fun indent 0 = ""
     | indent i = "   " ^ indent (i - 1)

   fun ppenv ind ((symb, CPair (VDec ty, cl)) :: t) =
      let
         val _ = print "hello\n"
      in
      indent ind ^ S.toString symb ^ " : " ^ T.ppty ty ^ "\n" ^
         ppenv (ind+1) cl ^ "\n" ^ ppenv ind t
      end
     | ppenv ind [] = ""

   fun prettyPrintEnv e = ppenv 0 e

   (* Built-in types *)
   val tyInt = T.TySimple (S.fromString "Int64.int" (0, "__builtin__"))
   val tyReal = T.TySimple (S.fromString "Real64.real" (0, "__builtin__"))
   val tyChar = T.TySimple (S.fromString "Char.char" (0, "__builtin__"))
   val tyString = T.TySimple (S.fromString "String.string" (0, "__builtin__"))
   val tyBool = T.TySimple (S.fromString "Bool.bool" (0, "__builtin__"))
   val tyUnit = T.TySimple (S.fromString "unit" (0, "__builtin__"))

   val freshTy = T.freshTy
   type id = int

   structure UF = IUnionFind

   val tysets : (id * (A.ty UF.set)) list ref = ref [] 

   fun getSet i = (case List.find (fn (i', _) => i = i') (!tysets) of
         SOME (_, s) => s
       | NONE => let val ns = UF.new (A.TyMeta i)
            in ns before tysets := (i, ns) :: (!tysets)
            end)
   fun lookup xs k = (case List.find (fn (j, _) => j = k) xs of
            SOME (_, v) => v
          | NONE => raise Fail ("Unbound variable " ^ A.ppty (A.TyMeta k)))

   fun force (A.TyMeta x) = UF.find (getSet x)
     | force t = t

   fun forceAll t =
   let fun aux (A.TyApp (t1, t2)) = A.TyApp (forceAll t1, forceAll t2)
        | aux (A.TyArrow (t1, t2)) = A.TyArrow (forceAll t1, forceAll t2)
        | aux t = t
   in aux (force t)
   end

   fun occursUF i t =
   (case t of
        A.TyArrow (t1, t2) => occursUF i (force t1) orelse occursUF i (force t2)
      | A.TyApp   (t1, t2) => occursUF i (force t1) orelse occursUF i (force t2)
      | A.TyMeta j => i = j
      | _ => false)

   fun pickCanon _ (A.TyMeta _, t) = t
     | pickCanon _ (t, A.TyMeta _) = t
     | pickCanon D (t1, t2) =
   if T.subt t1 t2 D then t1
   else raise Fail ("Non-substitution union called on " ^ A.ppty t1 ^ " and " ^ A.ppty t2)

   fun solve D (A.TyMeta x, A.TyMeta y) =
         UF.union (pickCanon D) (getSet x) (getSet y)
     | solve D (A.TyMeta x, tr) =
         if occursUF x tr then
            raise Fail ("Circular type constraints: " ^ 
               A.ppty (A.TyMeta x) ^ " in " ^ A.ppty tr)
         else UF.union (pickCanon D) (getSet x) (UF.new tr)
     | solve D (tl, tr as A.TyMeta x) = solve D (tr, tl)
     | solve D (t1 as A.TySimple a, t2 as A.TySimple b) =
         if T.subt t1 t2 D then () 
            else raise Fail ("Type error: " ^ A.ppty t1 ^ " =/= " ^ A.ppty t2)
     | solve D (A.TyVar a, A.TyVar b) =
         if a = b then () else raise Fail ("Polymorphic unification")
     | solve D (A.TyApp (t1, c1), A.TyApp (t2, c2)) =
         (solve D (force t1, force t2);
          solve D (force c1, force c2))
     | solve D (A.TyArrow (t1, t2), A.TyArrow (t3, t4)) =
         (solve D (force t1, force t3); solve D (force t2, force t4))
     | solve D (t1, t2) = raise Fail ("Type error: " ^ A.ppty t1 ^ " =/= " ^ A.ppty t2)

   fun solveList D xs = List.foldl (fn (c, ()) => solve D c) () xs

   fun mkPoly k t =
   let val src = ref 0
      val map : (int * int) list ref = ref []
      fun get x = (case List.find (fn (y, _) => x = y) (!map) of
             NONE => let val k = !src
                in k before (src := k + 1; map := (x, k) :: !map) end
            | SOME (_, k) => k)
      fun aux (t as A.TyMeta x) = if k > x then t else A.TyVar (get x)
        | aux (A.TyApp (t1, t2)) = A.TyApp (aux (force t1), aux (force t2))
        | aux (A.TyArrow (t1, t2)) = A.TyArrow (aux (force t1), aux (force t2))
        | aux t = t
   in aux t end


   fun infLiteral (A.Int _) = tyInt
     | infLiteral (A.Real _) = tyReal
     | infLiteral (A.Char _) = tyChar
     | infLiteral (A.String _) = tyString
     | infLiteral (A.Bool _) = tyBool
     | infLiteral (A.Unit) = tyUnit

   fun infExp env (literal as (A.LiteralExpr (l, p))) = 
      (infLiteral l, literal)
     | infExp env (A.Struct (decls, _, pos)) =
      let
         val (env', decls') = inferTypes env decls
         val t' = T.TySig () (* TODO: infer sig/kind *)
      in
         (t', T.Struct (decls', t', pos))
      end
     | infExp env exp = (T.TyArrow (freshTy (), freshTy ()),
         T.LiteralExpr(T.Unit, (0, "fake")))

   and infDec env (A.ValDec ((A.IdentifierPattern (symb, _, pos), exp), pos')) =
      let 
         val k = !tyvarCounter
	      val (t, e') = infExp env exp
	      val t' = mkPoly k (force t)
	   in 
         ((symb, CPair (VDec t', env)) :: env, 
            T.ValDec (
               (
                  A.IdentifierPattern (symb, t', pos), 
                  e'
               ), 
               pos'
            )
         )
	   end
     | infDec env (A.ValRecDec ((A.IdentifierPattern (symb, _, pos), e), pos')) =
      let 
         val k = !tyvarCounter
	      val t = freshTy ()
	      val (t', e') = infExp ((symb, CPair (VDec t, env)) :: env) e
	      val _ = solve env (force t, force t')
	      val t'' = mkPoly k (force t')
      in
         ((symb, CPair (VDec t'', env)) :: env, 
            T.ValRecDec ((T.IdentifierPattern (symb, t', pos), e'), pos'))
      end
     | infDec env (A.StructureMod (symb, body, pos)) =
      let
         val (t, e') = infExp env body
         val t' = t
         (* TODO: infKind *)
      in
         ((symb, CPair (VDec t', env)) :: env, A.StructureMod (symb, e', pos))
      end
     | infDec env exp = (env, exp)

   and inferTypes env decls =
   let
      val tDecls = 
         List.foldl (fn (decl, (env, decls)) =>
            let
               val _ = Debug.print Debug.debug
                  ("inferTypes: " ^ T.prettyPrint [decl])
               val (env', decl') = infDec env decl
            in
               (env', decls @ [decl'])
            end) (env, []) decls
   in
      tDecls
   end
end
