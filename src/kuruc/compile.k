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
*    The main compiler driver file that invokes each step of processing.
******************************************************************************)
structure Compile =
struct
   structure S = Symbol

   fun parseFile filename =
   let
      val p = ref 0
      val linepositions = ref [] : int list ref
      val _ = KuruParser.errorPos := 0

      fun readLines fp = 
         let
            val l = TextIO.inputLine fp
         in
            case l of NONE => ""
                    | SOME l' => (
                       linepositions := !linepositions @ [!p];
                       p := !p + size l';
                       l' ^ readLines fp)
         end


      val f = TextIO.openIn filename
      val buf = readLines f
      val _ = TextIO.closeIn f

      val parseResult = 
         KuruParser.kupeg_start buf handle Option => 
            raise Fail ("kuruc: error near " ^ 
                        (Error.resolveErrorPos(filename,
                                               !linepositions,
                                               KuruParser.error(),
                                               buf)))

      val _ = if KuruParser.error() + 20 < size buf then 
                 print ("kuruc: maybe error near " ^ 
                     (Error.resolveErrorPos(filename,
                                            !linepositions,
                                            KuruParser.error(),
                                            buf)) ^ "\n") else ()
	in
      map ParseTree.sanitise parseResult
   end

   
   fun deserialiseFile f = raise Fail "deserialiseFile not implemented"


   fun compile [] = raise Fail "No input files given"
     | compile fl = 
      let
         (* Stage 1 - Parse/Deserialise files *)
         val fl' = map (fn f => if String.isSuffix ".k" f then parseFile f
                           else if String.isSuffix ".ki" f then deserialiseFile f
                           else raise Fail ("Unknown file type: " ^ f)) fl

         (* Stage 2 - Parse tree passes *)
         val (te',ve',se',fl'') = 
                   List.foldl (fn (l,(te,ve,se,r)) => 
                       let
                          val (te',ve',se',r') = 
                              ConvertParseTree.convert te ve se l 
                       in
                          (te',ve',se',r @ r')
                       end) (Env.tenv_base, Env.venv_base, S.empty, []) fl'
         
         (* Stage 3 - Type inference/checking *)
         val (te'',ve'',se'',cs,fl''') = 
                   Constrain.generateConstraints te' ve' se' fl''

         val cs' = Unify.unify cs
         val _ = Debug.print Debug.internal ("Unified Constraints:\n")
         val _ = Debug.print Debug.internal (Unify.ppc cs' ^ "\n")

         val d = Unify.apply cs' fl'''

         val _ = Debug.print Debug.internal ("Typed program:\n")
         val _ = Debug.print Debug.internal ("\n" ^ TypedAST.prettyPrint d ^ "\n")

         (* Stage 4 - Optimisation *)

         (* Stage 5 - Typed Intermediate Representation *)

         val (ve''',se''',d',fragments) = Translate.translate d

         val _ = Debug.print Debug.internal ("Translated Program:\n")
         val _ = Debug.print Debug.internal ("\n" ^ 
                              Translate.printFragments fragments ^ "\n" ^
                              Intermediate.prettyPrint d' ^ "\n")

         (* Stage 6 - Code Generation (LLVM) *)

         (* Stage 7 - LLVM invokation *)

      in
         ()
      end
end
