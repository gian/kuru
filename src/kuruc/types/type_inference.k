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
******************************************************************************)

structure TypeInference =
struct
   structure S = Symbol
   structure A = TypedAst
   structure P = ParseTree

   (* datatype cdecl   = TDef of A.knd * A.ty | TDec of A.knd | VDec of A.ty
   datatype closure = CPair of cdecl * (S.symbol * closure) list
   type environment = (S.symbol * closure) list *)

   (* Built-in types *)
   val tyInt = A.TySimple (S.fromString "Int64.int" (0, "__builtin__"))
   val tyReal = A.TySimple (S.fromString "Real64.real" (0, "__builtin__"))
   val tyChar = A.TySimple (S.fromString "Char.char" (0, "__builtin__"))
   val tyString = A.TySimple (S.fromString "String.string" (0, "__builtin__"))
   val tyBool = A.TySimple (S.fromString "Bool.bool" (0, "__builtin__"))
   val tyUnit = A.TySimple (S.fromString "unit" (0, "__builtin__"))

   fun infLiteral (A.Int _) = tyInt
     | infLiteral (A.Real _) = tyReal
     | infLiteral (A.Char _) = tyChar
     | infLiteral (A.String _) = tyString
     | infLiteral (A.Bool _) = tyBool
     | infLiteral (A.Unit) = tyUnit

   fun env infDec (A.ValDec ((pat, exp), pos)) =
   let
      val _ = ()
   in
      ()
   end

   fun inferTypes decls =
   let
      val _ = ()
   in
      ("","")
   end
end
