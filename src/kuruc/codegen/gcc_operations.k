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
*    Utility module to invoke GCC.
******************************************************************************)
structure GCCOperations =
struct
   fun exec cmd args = OS.Process.system (cmd ^ " " ^ String.concatWith " " args)

   fun gcc (files,filename) =
      let
         val files' = files

         val args = ["-I"^String.concatWith ":" (Config.getLibPath()),
                                                  "-o",filename] @ files'

         val _ = Debug.print Debug.verbose
            ("GCC command line:\n" ^ 
            "gcc " ^ (String.concatWith " " args) ^ "\n")

         val ld = exec "gcc" args

         val _ = if not (OS.Process.isSuccess (ld))
                     then raise Fail 
                        ("gcc failed with error status code")
                     else ()
      in
         true
      end

   fun compile (sf,filename) =
      let
         val libkuru = Config.resolveLibrary "libkuru.a"
         val _ = Debug.print Debug.internal 
            ("GCCOperations.compile: libkuru.a: " ^ libkuru ^ "\n")
         val _ = gcc ([sf ^ "/main.c",libkuru],filename)
      in
         true
      end
end

