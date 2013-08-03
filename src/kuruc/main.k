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
             "\t-backend {c|llvm}\tSelect the backend to use.\n" ^
             "\t-keep-llvm\tKeep intermediate LLVM bitcode files.\n" ^
             "\t-m16\t\tSet machine word size to 16 bits.\n" ^
             "\t-m32\t\t(Default) Set machine word size to 32 bits.\n" ^
             "\t-m64\t\tSet machine word size to 64 bits.\n" ^
             "\n   C Backend Options:\n" ^
             "\t-cc-args <args>\tPass these arguments to the C compiler.\n" ^
             "\n   Libraries:\n" ^
             "\t-L [path]\tSet the library search path (colon-separated)\n" ^
             "\t-max-errors n\tReport no more than n errors.\n" ^
             "\t-safe\t\tImport only the Safe module, for sandbox environments\n"^
             "\t-no-basis\tDo not import the basis library.\n")
             

   fun printVersion () =
      print ("kuruc version " ^ Version.major ^ "." ^ Version.minor ^
             " build " ^ Version.build ^ " (" ^ Version.date ^ ") on " ^ 
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
     | parseArgs ("-O0"::t) = (Config.setOptimiser 0; parseArgs t)
     | parseArgs ("-O1"::t) = (Config.setOptimiser 1; parseArgs t)
     | parseArgs ("-O2"::t) = (Config.setOptimiser 2; parseArgs t)
     | parseArgs ("-O3"::t) = (Config.setOptimiser 3; parseArgs t)
     | parseArgs ("-m16"::t) = (Config.setMachineWord 16; parseArgs t)
     | parseArgs ("-m32"::t) = (Config.setMachineWord 32; parseArgs t)
     | parseArgs ("-m64"::t) = (Config.setMachineWord 64; parseArgs t)
     | parseArgs ("-keep-llvm"::t) = (Config.setKeepLLVM true; parseArgs t)
     | parseArgs ("-max-errors"::m::t) = (Config.setMaxErrors m; parseArgs t)
     | parseArgs ("-safe"::t) = (Config.setSafe true; parseArgs t)
     | parseArgs ("-no-basis"::t) = (Config.setBasis false; parseArgs t)
     | parseArgs ("-o"::f::t) = (Config.setOutputFile f; parseArgs t)
     | parseArgs ("-L"::f::t) = (Config.setLibPath f; parseArgs t)
     | parseArgs ("-backend"::f::t) = (Config.setBackend f; parseArgs t)
     | parseArgs ("-cc-args"::f::t) = (Config.setCCArgs f; parseArgs t)
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


