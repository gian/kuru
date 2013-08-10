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
*    Global config and configuration defaults.
******************************************************************************)

structure Config =
struct
   val outputFile = ref "KuruProg"
   val optimiserLevel = ref 2
   val machineWord = ref 32
   val keepLLVM = ref false
   val libPath = ref [] : string list ref
   val backend = ref "c"
   val ccargs = ref ""

   fun configureLibraryPaths () =
      libPath :=
      let
         val homeDir = (case OS.Process.getEnv "HOME" of
                           NONE => 
                              (Debug.print Debug.warning
                                ("Unable to read environment variable $HOME - " ^ 
                                "Are you sure we're in a sane environment?\n")
                               ;"")
                         | SOME t => t)

         val p = 
            (case OS.Process.getEnv "KURU_LIBRARY_PATH" of
               NONE => 
                  (Debug.print Debug.verbose 
                     "$KURU_LIBRARY_PATH is not set - using defaults.\n";
                   homeDir ^ "/.kuru/:/usr/local/lib/kuru/:/usr/lib/kuru/")
             | SOME x => x)

         val p' = String.tokens (fn #":" => true | _ => false) p
      in
         p'
      end
   val maxErrors = ref 30
   val safe = ref false
   val basis = ref true

   (* Setters and getters *)
   fun setOutputFile s = outputFile := s
   fun setOptimiser l = optimiserLevel := l
   fun setKeepLLVM l = keepLLVM := l
   fun setLibPath l = libPath := (String.tokens (fn #":" => true | _ => false) l)
   fun setMachineWord 16 = machineWord := 16
     | setMachineWord 32 = machineWord := 32
     | setMachineWord 64 = machineWord := 64
     | setMachineWord x = 
         raise Fail ("Invalid machine word size: " ^ Int.toString x)
   fun setMaxErrors m = 
      case (Int.fromString m) of NONE => 
            raise Fail "Invalid argument to -max-errors option"
         | SOME i => maxErrors := i
   fun setSafe t = safe := t
   fun setBasis t = basis := t
   fun setBackend t = backend := t
   fun setCCArgs t = ccargs := t

   fun getOutputFile () = !outputFile
   fun getTemporaryDirectory () =
    let
       val tmpDir = OS.FileSys.tmpName () ^ "_kuruc"
       val _ = OS.FileSys.mkDir tmpDir
    in
       tmpDir
    end

   fun getOptimiser () = !optimiserLevel
   fun getMachineWord () = !machineWord
   fun getLibPath () = !libPath
   fun getMaxErrors () = !maxErrors
   fun getSafe () = !safe
   fun getBasis () = !basis
   fun getBackend () = !backend
   fun getCCArgs () = !ccargs

   (* Resolve a full path to a library, based on the library path *)

   fun resolveLibrary n = 
     (case (List.find (fn x => 
                  (Debug.print Debug.internal ("resolveLibrary: " ^ x ^ "\n");
                   OS.FileSys.access (x,[OS.FileSys.A_READ])))
              (map (fn x => x ^ "/" ^ n) (getLibPath ()))) of
         NONE => raise Fail ("Could not locate library: " ^ n)
       | SOME l => l)
end
