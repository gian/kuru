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
*    Kuru Compiler (kuruc) program entry point and command line parser. 
******************************************************************************)

structure Main =
struct
   val args = CommandLine.arguments ()

   fun parseArgs [] = []
	  | parseArgs ("-v"::t) = (KuruParser.debugVerbose := true; 
                              parseArgs t)
     | parseArgs (h::t) = h :: parseArgs t
      
   val args' = parseArgs args 

   val _ = if length args' < 1 then 
            raise Fail "Usage: kuruc [-v] filename.k" else ()
	
   val linepositions = ref [] : int list ref
   val p = ref 0

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


   val filename = hd args'

   val f = TextIO.openIn filename
   val buf = readLines f
   val _ = TextIO.closeIn f

   val p = KuruParser.kupeg_start buf handle Option => 
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


   val pretty = (String.concatWith "\n" (map ParseTree.prettyPrint p))

   fun main () = print (pretty ^ "\n")
end   
   
val _ = Main.main ()


