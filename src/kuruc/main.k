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

	fun printUsage () =
		print ("Usage: kuruc [-vdqh] file1.k ... fileN.k\n" ^
		       "\tOptions:\n" ^
				 "\t\t-h\tDisplay this help information and exit.\n" ^
				 "\t\t-q\tQuiet mode - show only fatal errors.\n" ^
				 "\t\t-qq\tSilent mode - don't even show errors.\n" ^
				 "\t\t-v\tVerbose mode.\n" ^
				 "\t\t-vv\tBe more verbose.\n" ^
				 "\t\t-d\tEnable debugging output.\n" ^
				 "\t\t-dd\tEnable noisy debugging mode.\n" ^
				 "\t\t-V\tOutput version information and exit.\n")

	fun printVersion () =
		print ("kuruc version " ^ Version.major ^ "." ^ Version.minor ^
				 " build " ^ Version.build ^ " (" ^ Version.date ^ ") on " ^ 
				 Version.hostname ^ "\n" ^
		       "Copyright (C) 2010 Gian Perrone\n" ^ 
				 "http://www.kuru-lang.org\n")

   fun parseArgs [] = []
	  | parseArgs ("-v"::t) = (Debug.setDebugLevel Debug.verbose;
                              parseArgs t)
	  | parseArgs ("-vv"::t) = (Debug.setDebugLevel Debug.internal;
                              parseArgs t)
	  | parseArgs ("-d"::t) = (Debug.setDebugLevel Debug.debug;
                              parseArgs t)
	  | parseArgs ("-dd"::t) = (KuruParser.debugVerbose := true;
	  									Debug.setDebugLevel Debug.debugmore;
                              parseArgs t)
	  | parseArgs ("-q"::t) = (KuruParser.debugVerbose := false;
	  									Debug.setDebugLevel Debug.error;
                              parseArgs t)
	  | parseArgs ("-qq"::t) = (KuruParser.debugVerbose := false;
	  									Debug.setDebugLevel Debug.silent;
                              parseArgs t)
	  | parseArgs ("-V"::t) = (printVersion (); Unix.exit 0w0; raise Fail "")
	  | parseArgs ("-h"::t) = (printUsage (); Unix.exit 0w0; raise Fail "")
     | parseArgs (h::t) = h :: parseArgs t
  
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
		
		val _ = print ("Pre-Passes:\n" ^ 
								(String.concatWith "\n" (map ParseTree.prettyPrint parseResult)) ^ "\n")

      val (tenv',venv',astResult1) = ConvertParseTree.convertPasses
										Env.tenv_base Env.venv_base parseResult

	
		val _ = print ("Post-Passes:\n" ^ 
								(String.concatWith "\n" (map ParseTree.prettyPrint astResult1)) ^ "\n")


   	val (tenv'',venv'',senv'',astResult) = ConvertParseTree.convert 
										tenv' venv' Symbol.empty parseResult

      val _ = print "Untyped AST:\n"
      val _ = print (TypedAST.prettyPrint astResult)

      val (tenv''',venv''',senv''',constraints,typedAst) = 
            Constrain.generateConstraints tenv'' Env.venv_base senv'' astResult


      val _ = print "\nTyped AST:\n"
      val _ = print (TypedAST.prettyPrint typedAst) 

      val constraints' = Unify.unify constraints
      val _ = print "\nUnified Constraints:\n"
      val _ = print (Constrain.prettyPrint constraints')
      val _ = print "\n"

	   in
      map ParseTree.sanitise parseResult
   end


   fun main () =
   let
      val args' = parseArgs args 

      val _ = if length args' < 1 then (printUsage (); 
            raise Fail "Missing filename") else ()
	
      
      fun parseEach [] = ()
        | parseEach (h::t) = 
          let
             val _ = print ("File " ^ h ^ "\n")
             val p = parseFile h
             val pretty = (String.concatWith "\n" (map ParseTree.prettyPrint p))
          in
            parseEach t
          end
   in
      parseEach args'
   end
end   
   
val _ = Main.main ()


