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
*    Error reporting utility code.
******************************************************************************)

structure Error =
struct
   exception CompileError

   val gLinePositions = ref [] : (string * int list) list ref

   val errorCount = ref 0

   fun setLinePositions (filename,linePositions) =
      (gLinePositions := !gLinePositions @ [(filename,linePositions)])

   fun getLinePositions filename =
     (case (List.find (fn (f,l) => f = filename) (!gLinePositions)) of
         NONE => []
       | SOME (_,x) => x)

   fun resolveErrorPos (pos,filename) =
      let
         val k = ref 0
         fun l [] = (!k,0)
           | l (h::t) = 
           if pos <= h then (!k,h-pos) else (k := 1 + !k; l t)
         
         val (line,col) = l (getLinePositions filename)
      in
         filename ^ ":" ^ Int.toString line ^ ":" ^ Int.toString col
      end

   fun fail (msg,p) = 
      let
         val _ = if !errorCount >= Config.getMaxErrors () 
                     then (if Debug.getDebugLevel () = 0 then () else
                              TextIO.output(TextIO.stdErr,
                              "Aborted: too many errors.\n");
                           OS.Process.exit OS.Process.failure)
                     else ()

         val _ = errorCount := !errorCount + 1

         
         val _ = if Debug.getDebugLevel() = 0 then () else
                     TextIO.output(TextIO.stdErr,  
                               resolveErrorPos p ^ ": " ^ msg ^ "\n")

             in
         if !errorCount <= Config.getMaxErrors () then raise CompileError
            else raise Fail "Aborted: too many errors."
      end

   fun bug (module,msg,p) =
      (TextIO.output (TextIO.stdErr,
                  resolveErrorPos p ^ 
                  ": Code near this location triggered a compiler bug in " ^ 
                  "module " ^ module ^ ":\n   " ^ 
                  msg ^ "\nPlease consider reporting this at " ^
                  "http://www.kuru-lang.org/bugs\n");
       OS.Process.exit OS.Process.failure;
       raise Fail "Bug")

   fun getErrorCount () = !errorCount
end

