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
   val outputFile = ref "a.out"
   val optimiserLevel = ref 2
   val machineWord = ref 32
   val keepLLVM = ref false

   (* Setters and getters *)
   fun setOutputFile s = outputFile := s
   fun setOptimiser l = optimiserLevel := l
   fun setKeepLLVM l = keepLLVM := l

   fun setMachineWord 16 = machineWord := 16
     | setMachineWord 32 = machineWord := 32
     | setMachineWord 64 = machineWord := 64
     | setMachineWord x = 
         raise Fail ("Invalid machine word size: " ^ Int.toString x)

   fun getOutputFile () = !outputFile
   fun getOptimiser () = !optimiserLevel
   fun getMachineWord () = !machineWord
   fun getKeepLLVM () = !keepLLVM
end
