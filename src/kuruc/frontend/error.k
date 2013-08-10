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

