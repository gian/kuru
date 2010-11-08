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
structure Parse =
struct
   fun parseFile filename =
   let
      val p = ref 0
      val linepositions = ref [] : int list ref
      val _ = KdocParser.errorPos := 0

      fun readLines fp = 
         let
            val l = TextIO.inputLine fp
         in
            case l of NONE => ""
                    | SOME l' => (l' ^ readLines fp)
         end


      val f = TextIO.openIn filename
      val buf = readLines f
      val _ = TextIO.closeIn f

      val parseResult = 
         KdocParser.kupeg_start buf handle Option => 
				raise Fail "parse"
	in
		ParseTree.FileNode (filename, parseResult)
   end

   
   fun parse [] = raise Fail "No input files given"
     | parse fl = 
      let
         val fl' = map parseFile fl
      in
         fl'
      end
end
