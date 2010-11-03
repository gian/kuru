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
*    Convert a ParseTree to a TypedAST through successive steps of
*    simplification and elaboration.
******************************************************************************)
structure ConvertParseTree =
struct
	structure T = Type
	structure S = Symbol
   structure P = ParseTree
   structure D = Debug

   (* TODO: could replace this static list with something that respects
    * various command line parameters *)
   val passes = [
      ("ast-infix-unflatten",AstInfixUnflatten.apply),
      ("ast-app-apply",AstAppApply.apply),
      ("ast-infix-apply",AstInfixApply.apply),
      ("ast-list-cons",AstListCons.apply),
      ("ast-fun-curry",AstFunCurry.apply),
      ("ast-typedec-eval",AstTypedecEval.apply),
      ("ast-match-compile", AstMatchCompile.apply),
      ("ast-datatype-unfold", AstDatatypeUnfold.apply)
   ]

   fun convertPass (s,f) tenv venv p =
      let
         val _ = D.print D.verbose ("ParseTree conversion pass: " ^ s ^ "\n")
      in
         f tenv venv p
      end

   fun convertPasses tenv (venv : Env.enventry S.table) p =
      List.foldl 
         (fn (f,(tenv',venv',p')) => (
               Debug.print Debug.debugmore (ParseTree.prettyPrintAll p')
               ; convertPass f tenv' venv' p'))
            (tenv,venv,p)
               passes

   fun convert tenv (venv : Env.enventry S.table) senv p =
      let
         (* Run all the ParseTree -> ParseTree passes *)
         val (tenv',venv',p') = convertPasses tenv venv p
         val _ = D.print D.verbose ("ParseTree -> TypedAST conversion\n")
      in
         AstParseTree.convert tenv' venv' senv p'
      end


end


