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

      val _ = Debug.print Debug.debugmore 
         ("Contents of file: " ^ filename ^ ":\n" ^ buf)

      val parseResult = 
         KuruParser.kupeg_start buf handle Option => 
            raise Fail ("kuruc: error near " ^ 
                        (Error.resolveErrorPos(filename,
                                               !linepositions,
                                               KuruParser.error(),
                                               buf)))

      val _ = if KuruParser.error() + 2 < size buf then 
                 print ("kuruc: maybe error near " ^ 
                     (Error.resolveErrorPos(filename,
                                            !linepositions,
                                            KuruParser.error(),
                                            buf)) ^ "\n") else ()
	in
      map ParseTree.sanitise parseResult
   end

   
   fun deserialiseFile f = raise Fail "deserialiseFile not implemented"

   fun parseBuildFile [] = []
     | parseBuildFile (h::t) = 
       let
          fun ws (#" "::t) = ws t
            | ws (#"\t"::t) = ws t
            | ws (#"#"::t) = []
            | ws x = x

          val h' = String.implode (ws (String.explode h))
       in
          if h' = "" then parseBuildFile t else
          if String.isPrefix "$/" h' 
                     then 
                          Config.resolveLibrary (String.extract(h',1,NONE)) ::
                           parseBuildFile t
                     else h' :: parseBuildFile t
       end
      

   fun compile [] = raise Fail "No input files given"
     | compile fl = 
      let
         (* Stage 1 - Parse/Deserialise files *)
         fun genInputs fl = 
              map (fn f => if (String.isSuffix ".k" f) orelse
                              (String.isSuffix ".ks" f) then parseFile f
                           else if String.isSuffix ".ki" f then deserialiseFile f
                           else if String.isSuffix ".kb" f then
                           let
                              val _ = Debug.print Debug.verbose
                                 ("Reading from Kuru build file: " ^ f ^ "\n")
                              val fp = TextIO.openIn f
                              val cont = TextIO.input fp
                              val cont' = 
                                 String.fields (fn #"\n" => true | _ => false) cont
                              val cont'' = parseBuildFile cont'
                              val _ = TextIO.closeIn fp
                           in
                              List.concat (genInputs cont'')
                           end
                           else raise Fail ("Unknown file type: " ^ f)) fl

         val fl' = genInputs fl

         (* Stage 2 - Parse tree passes *)
         val (te',ve',se',fl'') = 
                   List.foldl (fn (l,(te,ve,se,r)) => 
                       let
                          val (te',ve',se',r') = 
                              ConvertParseTree.convert te ve se l 
                          val _ = Debug.print Debug.debugmore (
                                       "Converted Typed AST:\n" ^ 
                                       TypedAST.prettyPrint r'
                                    )
                       in
                          (te',ve',se',r @ r')
                       end) (Env.tenv_base, Env.venv_base, Env.senv_base, []) fl'
         
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

         val (te''',ve''',se''',d',fragments) = Translate.translate te'' d

         val _ = Debug.print Debug.internal ("Translated Program:\n")
         val _ = Debug.print Debug.internal ("\n" ^ 
                              Translate.printFragments fragments ^ "\n" ^
                              Intermediate.prettyPrint d' ^ "\n")

         (* Stage 6 - Code Generation (LLVM) *)

         val code = CodeGen.codegen (d',fragments)
         val _ = Debug.print Debug.internal ("Generated code:\n" ^ code ^ "\n")
     
         val codeLocation = CodeGen.emit code

         (* Stage 7 - LLVM invokation *)
         val _ = LLVMOperations.compile (codeLocation,Config.getOutputFile ())
      in
         ()
      end
end
