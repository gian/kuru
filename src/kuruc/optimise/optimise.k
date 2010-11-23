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
*    The main optimiser stage that invokes other optimisation passes.
******************************************************************************)
structure Optimise =
struct
   structure I = Intermediate
   structure S = Symbol
   structure T = Type

   val passes = ref [] : (int * string * (I.ir -> I.ir)) list ref

   fun registerOptimisation lvl name f =
      passes := !passes @ [(lvl,name,f)]

   fun optimise ir =
   let
      val _ = Debug.startStage 0 "Optimise"

      val opts = 
         List.filter (fn (l,_,_) => l <= Config.getOptimiser ()) (!passes)

      val _ = Debug.print Debug.verbose 
         (Int.toString (length opts) ^ " optimisations enabled.\n")

      val ir' = 
         List.foldl 
            (fn ((i,s,f),ir) =>
            let
               val _ = Debug.startStage 1 ("OptimisationPass: " ^ s)
               val szPre = I.size ir

               val ir' = f ir

               val szPost = I.size ir'
               val _ = Debug.endStage ("OptimisationPass: " ^ s)
               val _ = Debug.print Debug.internal
                  ("Optimisation pass '"^s^"' changed size from " ^ 
                     Int.toString szPre ^ " ==> " ^ Int.toString szPost ^
                     "\n")
            in
               ir'
            end) ir opts

      val _ = Debug.endStage "Optimise"
   in
      ir'
   end
end

