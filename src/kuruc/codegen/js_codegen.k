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
*    C backend code generator.
******************************************************************************)
structure JSCodeGen =
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
         jnl (map jnl pre),
         "$(document).ready(function () {\n",
         "   io_init ();",
         "   int_init ();",
         "   string_init ();",
         jnl (map (ind 1) stms),
         "   io_done ();",
         "   int_done ();",
         "   string_done ();",
         "\n});\n"
      ]

   fun emit code =
      let
         val fp = TextIO.openOut (Config.getOutputFile())

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

         val _ = Debug.print Debug.verbose ("Output JS file: " ^ 
                                             Config.getOutputFile())

      in
         Config.getOutputFile()
      end handle OS.SysErr (m,NONE) => raise Fail ("Cannot emit code: " ^ m)
               | OS.SysErr (m,SOME se) => raise Fail ("Cannot emit code: " ^ 
                                                      m ^ ": " ^ OS.errorMsg se)
end

