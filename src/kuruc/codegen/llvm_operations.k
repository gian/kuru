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
   fun exec cmd args = Unix.execute (cmd, args)
   
   fun llvmas (llvmSource) = 
      let
         val llas = exec "/opt/local/bin/llvm-as" []
         val (fi,fo) = (fn x => (Unix.binInstreamOf x, Unix.textOutstreamOf x)) llas
         val _ = TextIO.output (fo,llvmSource)
         val _ = TextIO.closeOut fo
         val asm = BinIO.inputAll fi
         val _ = BinIO.closeIn fi

         val _ = if not (OS.Process.isSuccess (Unix.reap llas))
                     then raise Fail 
                        ("llvm-as failed with error status code")
                     else ()
      in
         asm
      end
   
   fun opt (llvmBytecode) =
      let
         val llo = exec "/opt/local/bin/opt" ["-tailcallelim","-O2"]
         val (fi,fo) = (fn x => (Unix.binInstreamOf x, Unix.binOutstreamOf x)) llo
         val _ = BinIO.output (fo,llvmBytecode)
         val _ = BinIO.closeOut fo
         val bc = BinIO.inputAll fi
         val _ = BinIO.closeIn fi

         val _ = if not (OS.Process.isSuccess (Unix.reap llo))
                     then raise Fail 
                        ("opt failed with error status code")
                     else ()
      in
         bc
      end

   fun llc (llvmBytecode,filename) =
      let
         val llc = exec "/opt/local/bin/llc" ["-f","-o",filename]
         val (fi,fo) = (fn x => (Unix.binInstreamOf x, Unix.binOutstreamOf x)) llc
         val _ = BinIO.output (fo,llvmBytecode)
         val _ = BinIO.closeOut fo
         val bc = BinIO.inputAll fi
         val _ = BinIO.closeIn fi

         val _ = if not (OS.Process.isSuccess (Unix.reap llc))
                     then raise Fail 
                        ("llc failed with error status code")
                     else ()
      in
         true
      end

   (* TODO: This needs to finish linking against the runtime *)
   fun compile (llvmSource,filename) =
      let
         val bc = llvmas llvmSource
         val bc' = opt bc
         val _ = llc (bc',filename) 
      in
         true
      end

end
