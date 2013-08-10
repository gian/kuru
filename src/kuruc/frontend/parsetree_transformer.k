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
*    Run all the transformation passes on the parse tree.
*******************************************************************************)

structure ParseTreeTransformer =
struct
   structure P = ParseTree
   structure D = Debug

   val passes = [
      ("parsetree-infix-unflatten",ParseTreeInfixUnflatten.apply),
      ("parsetree-app-apply",ParseTreeAppApply.apply),
      ("parsetree-infix-apply",ParseTreeInfixApply.apply),
      ("parsetree-infix-pattern",ParseTreeInfixPattern.apply),
      ("parsetree-list-cons",ParseTreeListCons.apply),
      ("parsetree-promote-exns",ParseTreePromoteExns.apply)
   ]

   fun convertPass (s,f) tenv venv p =
      let
         val _ = D.print D.internal ("ParseTree conversion pass: " ^ s ^ "\n")
      in
         f tenv venv p
      end

   fun convertPasses tenv venv p =
      List.foldl
         (fn (f,(tenv',venv',p')) => (
               Debug.print Debug.debugmore (ParseTree.prettyPrintAll p')
               ; convertPass f tenv' venv' p'))
            (tenv,venv,p)
               passes

   fun transform parseTree = 
      (fn (_,_,pt') => pt') (convertPasses () () parseTree)
end
