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
*    Wraps LLVM operations.
******************************************************************************)
structure LLVMOperations =
struct
   (* FIXME: This should be in an external platform-independent file *)
   fun exec cmd args = OS.Process.system (cmd ^ " " ^ String.concatWith " " args)
   
   fun llvmas (llvmSourceLoc) = 
      let
         val llas = exec "llvm-as" ["-f", llvmSourceLoc ^ "/main.ll"]

         val _ = if not (OS.Process.isSuccess (llas))
                     then raise Fail 
                        ("llvm-as failed with error status code")
                     else ()
      in
         llvmSourceLoc ^ "/main.bc"
      end
   
   fun opt (llvmBytecode) =
      let
         val llo = exec "opt" [
                                              "-O" ^ (Int.toString
                                                (Config.getOptimiser ()))]
         val _ = if not (OS.Process.isSuccess (llo))
                     then raise Fail 
                        ("opt failed with error status code")
                     else ()
      in
         () 
      end

   fun llc (llvmBytecodeLoc) =
      let
         val filename' = llvmBytecodeLoc ^ ".s"
         val llc = exec "llc" 
                    (["-f","-o",filename'] @ 
                      (if (Config.getOptimiser () > 0) then
                            ["-O" ^ Int.toString (Config.getOptimiser ())]
                      else []) @ [llvmBytecodeLoc])
         
         val _ = if not (OS.Process.isSuccess (llc))
                     then raise Fail 
                        ("llc failed with error status code")
                     else ()
      in
         filename'
      end


   fun gcc (files,filename) =
      let
         val files' = files 
         val ld = exec "llvm-gcc" ([
                                                  "-o",filename] @ files')

         val _ = if not (OS.Process.isSuccess (ld))
                     then raise Fail 
                        ("gcc failed with error status code")
                     else ()
      in
         true
      end

   fun llvmld (files,filename) =
      let
         val files' = files 
         val ld = exec "llvm-ld" (["-native",
                                                  "-o",filename] @ files')

         val _ = if not (OS.Process.isSuccess (ld))
                     then raise Fail 
                        ("gcc failed with error status code")
                     else ()
      in
         true
      end

   (* TODO: This needs to finish linking against the runtime *)
   fun compile (llvmSourceLocation,filename) =
      let
         val _ = Debug.print Debug.internal 
            ("LLVMOperations.compile: " ^ llvmSourceLocation ^ "\n")
         val bc = (llvmas llvmSourceLocation)
         val _ = Debug.print Debug.internal 
            ("LLVMOperations.compile: bitcode: " ^ bc ^ "\n")
         val sf = llc bc
         val _ = Debug.print Debug.internal 
            ("LLVMOperations.compile: asm: " ^ sf ^ "\n")
         val libkuru = Config.resolveLibrary "libkuru.a"
         val _ = Debug.print Debug.internal 
            ("LLVMOperations.compile: libkuru.a: " ^ libkuru ^ "\n")
         val _ = gcc ([sf,libkuru],filename) 

      in
         true
      end

end
