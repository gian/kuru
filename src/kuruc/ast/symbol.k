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
*    Symbol library for identifiers and operations on them.
******************************************************************************)

structure Symbol :> SYMBOL =
struct
   type pos = Position.t

   val symbolId = ref 0
   fun nextSymbolId () = (symbolId := !symbolId + 1; !symbolId)

   datatype identifier =
      Short of string * int * pos
    | Long of string list * int * pos

   type t = identifier

   fun toString (Short (s,_,_)) = s
     | toString (Long (l,_,_)) = String.concatWith "." l

   fun fromString s pos =
      case String.fields (fn #"." => true | _ => false) s of
         [t] => Short (t, nextSymbolId (), pos)
       | l => Long (l, nextSymbolId(), pos)

   (* This isn't a good hashing strategy.  It's a terrible one. *)
   fun prime 0 = 11 
     | prime 1 = 13
     | prime 2 = 17
     | prime 3 = 19
     | prime 4 = 23
     | prime 5 = 29
     | prime 6 = 31
     | prime 7 = 37
     | prime 8 = 41
     | prime 9 = 43
     | prime 10 = 47
     | prime 11 = 53
     | prime 12 = 59
     | prime 13 = 61
     | prime 14 = 67
     | prime 15 = 71
     | prime _ = 131

   fun hash (Short (s, _, _)) =
         Vector.foldli (fn (i, c, h) => h + prime i * Char.ord c) 0 s
     | hash (Long ([s], _, _)) =
         Vector.foldli (fn (i, c, h) => h + prime i * Char.ord c) 0 s
     | hash (Long (h::t, x, y)) = hash (Long (t, x, y))

   fun equals (Short (s, _, _), Short (s', _, _)) = s = s'
     | equals (Long (l, _, _), Long (l', _, _)) = l = l'
     | equals (Short (s, _, _), Long ([s'], _, _)) = s = s'
     | equals (Long ([s], _, _), Short (s', _, _)) = s = s'
     | equals _ = false

   fun isLong (Long ([i], _, _)) = false
     | isLong (Long _) = true
     | isLong _ = false

   fun hd (Long (l, i, p)) = Short (List.hd l, i, p)
     | hd s = s

   fun tl (Long (h::t, i, p)) = Long (t, i, p)
     | tl (Long ([h], i, p)) = Short (h, i, p)
     | tl (Long (_, _, p)) = Error.bug ("Symbol.tl", "Called tl on empty long-id", p)
     | tl (Short (_, _, p)) = Error.bug ("Symbol.tl", "Called tl on non long-id", p)

   fun getPos (Short (_, _, p)) = p
     | getPos (Long (_, _, p)) = p

end

