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
*    Kuru Compiler (kuruc) program entry point and command line parser. 
******************************************************************************)

structure Main =
struct
   val args = CommandLine.arguments ()

   fun printUsage () =
      print ("Usage: kuruc [options] [-o outputFile] file1.k ... fileN.k\n" ^
             "   Global Options:\n" ^
             "\t-o file\t\tSpecify the output file.\n" ^
             "\t-h\t\tDisplay this help information and exit.\n" ^
             "\t-V\t\tOutput version information and exit.\n" ^
             "\n   Optimisation:\n"^
             "\t-O0\t\tDisable all optimisations.\n" ^ 
             "\t-O1\t\tEnable basic optimisations.\n" ^ 
             "\t-O2\t\t(Default) Enable most optimisations.\n" ^ 
             "\t-O3\t\tEnable all optimisations.\n" ^
             "\n   Verbosity:\n" ^ 
             "\t-q\t\tQuiet mode - show only fatal errors.\n" ^
             "\t-qq\t\tSilent mode - don't even show errors.\n" ^
             "\t-v\t\tVerbose mode.\n" ^
             "\t-vv\t\tBe more verbose.\n" ^
             "\t-d\t\tEnable debugging output.\n" ^
             "\t-dd\t\tEnable noisy debugging mode.\n" ^
             "\n   Compilation:\n" ^
             "\t-backend {java}\tSelect the backend to use.\n" ^
             "\n   Javac Backend Options:\n" ^
             "\t-javac-args <args>\tPass these arguments to the javac compiler.\n" ^
             "\n   Libraries:\n" ^
             "\t-cp [path]\tSet the class path (colon-separated)\n" ^
             "\t-max-errors n\tReport no more than n errors.\n" ^
             "\t-safe\t\tImport only the Safe module, for sandbox environments\n"^
             "\t-no-basis\tDo not import the basis library.\n")
             

   fun printVersion () =
      print ("kuruc version " ^ 
         Version.major ^ "." ^ Version.minor ^ "." ^ Version.build ^ "\n" ^
             "build " ^ Version.gitId ^ " (" ^ Version.date ^ ") on " ^ 
             Version.hostname ^ "\n" ^
             "Copyright (C) 2010-2013 Gian Perrone\n" ^ 
             "http://www.kuru-lang.org\n")

   fun parseArgs [] = []
     | parseArgs ("-v"::t) = (Debug.setDebugLevel Debug.verbose;
                              parseArgs t)
     | parseArgs ("-vv"::t) = (Debug.setDebugLevel Debug.internal;
                              parseArgs t)
     | parseArgs ("-d"::t) = (Debug.setDebugLevel Debug.debug;
                              parseArgs t)
(*     | parseArgs ("-dd"::t) = (KuruParser.debugVerbose := true;
                               Debug.setDebugLevel Debug.debugmore;
                               parseArgs t)
     | parseArgs ("-q"::t) = (KuruParser.debugVerbose := false;
                              Debug.setDebugLevel Debug.error;
                              parseArgs t)
     | parseArgs ("-qq"::t) = (KuruParser.debugVerbose := false;
                               Debug.setDebugLevel Debug.silent;
                               parseArgs t) *)
     | parseArgs ("-V"::t) = (printVersion (); Unix.exit 0w0; raise Fail "")
     | parseArgs ("-h"::t) = (printUsage (); Unix.exit 0w0; raise Fail "")
     | parseArgs ("-O0"::t) = (Config.setOptimiser 0; parseArgs t)
     | parseArgs ("-O1"::t) = (Config.setOptimiser 1; parseArgs t)
     | parseArgs ("-O2"::t) = (Config.setOptimiser 2; parseArgs t)
     | parseArgs ("-O3"::t) = (Config.setOptimiser 3; parseArgs t)
     | parseArgs ("-max-errors"::m::t) = (Config.setMaxErrors m; parseArgs t)
     | parseArgs ("-safe"::t) = (Config.setSafe true; parseArgs t)
     | parseArgs ("-no-basis"::t) = (Config.setBasis false; parseArgs t)
     | parseArgs ("-o"::f::t) = (Config.setOutputFile f; parseArgs t)
     | parseArgs ("-cp"::f::t) = (Config.setLibPath f; parseArgs t)
     | parseArgs ("-backend"::f::t) = (Config.setBackend f; parseArgs t)
     | parseArgs ("-javac-args"::f::t) = (Config.setCCArgs f; parseArgs t)
     | parseArgs (h::t) = h :: parseArgs t
  
   fun main () =
   let
      val args' = parseArgs args 

      val _ = if length args' < 1 then (printUsage (); 
            raise Fail "Missing filename") else ()
   
   in
      Compile.compile args'
   end
end   
   
val _ = Main.main () handle Fail s => (if Debug.getDebugLevel () = 0 then () else
         TextIO.output(TextIO.stdErr, "Exception: " ^ s ^ "\n");
         OS.Process.exit OS.Process.failure)

val _ = OS.Process.exit OS.Process.success


