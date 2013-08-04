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
   fun getKeepLLVM () = !keepLLVM
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
