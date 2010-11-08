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
*    Kuru Documentation generator program entry point and command line parser. 
******************************************************************************)

structure Version =
struct
	val major = "0"
	val minor = "1"
end

structure Main =
struct
   val args = CommandLine.arguments ()

	val outputDir = ref "doc.xml"

	fun printUsage () =
		print ("Usage: kdoc [options] [-o outputFile] file1.k ... fileN.k\n" ^
		       "   Options:\n" ^
             "\t-o file\t\tSpecify the output file.\n" ^
				 "\t-V\t\tOutput version information and exit.\n")

	fun printVersion () =
		print ("kdoc version " ^ Version.major ^ "." ^ Version.minor ^
				 "\n" ^
		       "Copyright (C) 2010 Gian Perrone\n" ^ 
				 "http://www.kuru-lang.org\n")

   fun parseArgs [] = []
	  | parseArgs ("-V"::t) = (printVersion (); Unix.exit 0w0; raise Fail "")
     | parseArgs ("-o"::f::t) = (outputDir := f; parseArgs t)
     | parseArgs (h::t) = h :: parseArgs t
  
   fun main () =
   let
      val args' = parseArgs args 

      val _ = if length args' < 1 then (printUsage (); 
            raise Fail "Missing filename") else ()
	
		val pt = Parse.parse args'
	in
		DocGen.genDocs (!outputDir) pt
   end
end   
   
val _ = Main.main ()


