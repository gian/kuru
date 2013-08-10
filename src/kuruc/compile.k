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
*    Controls the main sequence of compilation from start to finish.
******************************************************************************)
structure Compile =
struct

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
         val fl'' = map ParseTreeTransformer.transform fl'
         val _ = Debug.print Debug.debug (String.concatWith "\n\n" 
            (map (fn l => String.concatWith "\n" (map ParseTree.prettyPrint l)) fl'') ^ "\n")
         val _ = Debug.endStage "ParseTreeTransform"

         (* Stage 3 - Transform to TypedAST *)
         val typedAst = ParseTreeAstConverter.convert fl''

         val _ = Debug.print Debug.debug (TypedAst.prettyPrint typedAst)
      in
         ()
      end
end

