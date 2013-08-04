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
      val _ = Debug.startStage 2 ("parsing " ^ filename)
      val p = ref 0
      val linepositions = ref [] : int list ref
      val _ = KuruParser.errorPos := 0
      val _ = KuruParser.kuruFile := (OS.Path.mkCanonical filename)

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

      val _ = Error.setLinePositions (OS.Path.mkCanonical filename,!linepositions)

      val _ = Debug.print Debug.debugmore 
         ("Contents of file: " ^ filename ^ ":\n" ^ buf)

      val parseResult = 
         KuruParser.kupeg_start buf handle Option => 
            raise Fail ("kuruc: syntax error near " ^ 
                        (Error.resolveErrorPos(KuruParser.error (),
                                               OS.Path.mkCanonical filename)))

      val _ = if KuruParser.error() + 3 < size buf then 
            raise Fail ("kuruc: syntax error near " ^ 
                        (Error.resolveErrorPos(KuruParser.error (),
                                               OS.Path.mkCanonical filename)))
              else ()
      val _ = Debug.endStage ("parsing " ^ filename)
   in
      map ParseTree.sanitise parseResult
   end

   
   fun deserialiseFile f = raise Fail "deserialiseFile not implemented"

   (* FIXME: Something should prevent infinite cycles of build files. *)
   fun parseBuildFile _ [] = []
     | parseBuildFile pre (h::t) = 
       let
          fun ws (#" "::t) = ws t
            | ws (#"\t"::t) = ws t
            | ws (#"#"::t) = []
            | ws x = x

          val h' = String.implode (ws (String.explode h))

          val pre' = if pre = "" then "." else pre
       in
          if h' = "" then parseBuildFile pre t else
          if String.isPrefix "$/" h' 
                     then 
                          Config.resolveLibrary (String.extract(h',1,NONE)) ::
                           parseBuildFile pre t
                     else (pre' ^ "/" ^ h') :: parseBuildFile pre t
       end
      

   fun compile [] = raise Fail "No input files given"
     | compile fl = 
      let
         val _ = Debug.startStage 0 "Compile"

         (* Stage 0 - Configure library paths *)
         val _ = Debug.startStage 1 "LibraryPaths"
         val _ = Config.configureLibraryPaths ()
         val _ = Debug.endStage "LibraryPaths"

         (* Stage 1 - Parse/Deserialise files *)
         fun genInputs fl = 
              map (fn f => if (String.isSuffix ".k" f) orelse
                              (String.isSuffix ".ks" f) then parseFile f
                           else if String.isSuffix ".ki" f then deserialiseFile f
                           else if String.isSuffix ".kb" f then
                           let
                              val _ = Debug.print Debug.verbose
                                 ("Reading from Kuru build file: " ^ f ^ "\n")

                              val pre = OS.Path.getParent f

                              val _ = Debug.print Debug.verbose
                                 ("Making buildfile paths relative to: " ^pre ^"\n")
                              val fp = TextIO.openIn f
                              val cont = TextIO.input fp
                              val cont' = 
                                 String.fields (fn #"\n" => true | _ => false) cont
                              val cont'' = parseBuildFile pre cont'
                              val _ = TextIO.closeIn fp
                           in
                              List.concat (genInputs cont'')
                           end
                           else raise Fail ("Unknown file type: " ^ f)) fl

         val bs = if Config.getBasis () andalso (not (Config.getSafe ()))
                     then genInputs [(Config.resolveLibrary "kuru.kb")]
                     else if Config.getSafe () 
                        then genInputs [(Config.resolveLibrary "safe.kb")]
                     else []

         val _ = Debug.startStage 1 "GenInputs"
         val fl' = bs @ genInputs fl
         val _ = Debug.endStage "GenInputs"

         (* Stage 2 - Parse tree passes *)
         val _ = Debug.startStage 1 "ParseTreeTransform"
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
                          (te',ve',se',r @ [r'])
                       end) (Env.tenv_base, Env.venv_base, Env.senv_base, []) fl'

         val _ = Debug.endStage "ParseTreeTransform"

         (* Stage 3 - Type inference/checking *)
         val _ = Debug.startStage 1 "TypeInference"
         val fl''' = List.concat (map TypeCheck.check fl'')        
         val _ = Debug.endStage "TypeInference"

         val _ = Debug.print Debug.internal ("Typed program:\n")
         val _ = Debug.print Debug.internal 
            ("\n" ^ TypedAST.prettyPrint fl''' ^ "\n")

         val _ = Debug.startStage 1 "Monomorphisation"
         val flmono = Monomorphise.monomorphise fl'''
         val _ = Debug.endStage "Monomorphisation"

         val _ = Debug.print Debug.internal ("Monomorphic program:\n")
         val _ = Debug.print Debug.internal 
            ("\n" ^ Intermediate.prettyPrint flmono ^ "\n")

         (* Stage 4 - Optimisation *)

         (* val optfl = Optimise.optimise flmono *)

         (* Stage 5 - Closure conversion *)

         
         (* val _ = Debug.startStage 1 "ClosureConversion"
         val fcc = ClosureConvert.convert flmono 
         val _ = Debug.print Debug.internal ("Closure converted program:\n")
         val _ = Debug.print Debug.internal 
            ("\n" ^ Intermediate.prettyPrint (fcc) ^ "\n")
         val _ = Debug.endStage "ClosureConversion"
        *)

         (* Error Checking State: We proceed no further if errors occurred. *)
         val _ = if Error.getErrorCount () > 0 then 
                  raise Fail ("Errors: " ^ Int.toString (Error.getErrorCount()))
                  else ()

      in
         compileJava (flmono)
      end

      and compileJava fcc =
      let
         val _ = Debug.print Debug.verbose 
            "Java code generator backend selected\n"

         val _ = Debug.startStage 1 "JavaTranslate"
         val (seg,d') = JavaTranslate.translate fcc
         val _ = Debug.endStage "JavaTranslate"
         
         val _ = Debug.startStage 1 "CodeGen"
         val code = JavaCodeGen.codegen (seg, d')
         val _ = Debug.print Debug.internal ("Generated code:\n" ^ code ^ "\n")
     
         val _ = Debug.startStage 2 "Emit"
         val (srcPath, codeLocation) = JavaCodeGen.emit code
         val _ = Debug.endStage "Emit"

         val _ = Debug.endStage "CodeGen"
         
         (* Stage 7 - Javac invocation *)
         val _ = Debug.startStage 1 "javac"
         val _ = JavacOperations.compile (codeLocation, Config.getOutputFile ())
         val _ = Debug.endStage "javac"

         (* Stage 7 - JAR invocation *)
         val _ = Debug.startStage 1 "jar"
         val _ = JavacOperations.jar (srcPath, Config.getOutputFile () ^ ".jar")
         val _ = Debug.endStage "jar"

         val _ = Debug.endStage "Compile"
      in
         ()
      end
end
