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
*    Java backend code generator.
******************************************************************************)
structure JavaCodeGen =
struct
   val j = String.concat
   val jsp = String.concatWith " "
   val jnl = String.concatWith "\n"
   val jc = String.concatWith ", "

   fun ind' 0 = ""
     | ind' n = "   " ^ ind' (n-1)

   fun ind n s = 
      ind' n ^ s

   fun codegen (pre, stms) =
      jnl [
         "import kuru.Kuru;",
         jnl (map jnl pre),
         "public class " ^ Config.getOutputFile() ^ " {\n",
         "   public static void main(String args[]) {",
         jnl (map (ind 2) stms),
         "   }",
         "}",
         ""
      ]

   fun emit code =
      let
         val codeDir = Config.getTemporaryDirectory ()
         val srcPath = codeDir ^ "/" ^ Config.getOutputFile () ^ ".java"
         val fp = TextIO.openOut (srcPath)

         val manifest = TextIO.openOut (codeDir ^ "/manifest.mf")

         val _ = TextIO.output (manifest, "Main-Class: " ^ Config.getOutputFile () ^ "\n")

         val _ = TextIO.closeOut manifest

         fun includeLib s = 
            let
               val g = Config.resolveLibrary s 
               val lib = TextIO.openIn g
               val lib' = TextIO.input lib
               val _ = TextIO.closeIn lib
               val _ = TextIO.output (fp, lib' ^ "\n")
            in
               ()
            end
         
        (* val _ = includeLib "io_kuru.js"
         val _ = includeLib "int_kuru.js"
         val _ = includeLib "string_kuru.js" *)
         
         (* Final code emit *)
         val _ = TextIO.output (fp, code)
         val _ = TextIO.closeOut (fp)

         val _ = Debug.print Debug.verbose ("Output Java file: " ^ 
                                             Config.getOutputFile())

      in
         (codeDir, srcPath)
      end handle OS.SysErr (m,NONE) => raise Fail ("Cannot emit code: " ^ m)
               | OS.SysErr (m,SOME se) => raise Fail ("Cannot emit code: " ^ 
                                                      m ^ ": " ^ OS.errorMsg se)
end

