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
*    A crude hash map for symbol keys.
******************************************************************************)

signature SYMBOL_MAP =
sig
   type 'a map

   val put : 'a map -> Symbol.t -> 'a -> 'a map
   val get : 'a map -> Symbol.t -> (Symbol.t * 'a) option
   val values : 'a map -> (Symbol.t * 'a) list
   val empty : unit -> 'a map
   val size : 'a map -> int
end

structure SymbolMap :> SYMBOL_MAP =
struct
   val buckets = 79

   type 'a map = (Symbol.t * 'a) list vector

   fun empty () = Vector.tabulate (buckets, fn _ => [])

   fun put map key value =
   let
      val _ = Debug.print Debug.debugmore 
         ("SymbolMap.put: " ^ Symbol.toString key ^ "\n")
      val index = (Symbol.hash key) mod buckets
      val bucket = Vector.sub (map, index)
      val _ = Debug.print Debug.debugmore ("SymbolMap.put: bucket " ^
         Int.toString index ^ " of size " ^ Int.toString (length bucket) ^ "\n")
      val bucket' = (key, value) :: List.filter 
         (fn (k,v) => not (Symbol.equals (k, key))) bucket
      val map' = Vector.update (map, index, bucket')
   in
      map'
   end

   fun get map key = 
   let
      val index = (Symbol.hash key) mod buckets
      val _ = Debug.print Debug.debugmore ("SymbolMap.get: " ^ Int.toString index ^ "\n")
   in
      List.find (fn (s,_) => Symbol.equals(s,key)) (Vector.sub (map, index))
   end

   fun values map = 
      Vector.foldl (fn (bucket, l) =>
         l @ bucket) [] map

   fun size map = List.length (values map)

end

