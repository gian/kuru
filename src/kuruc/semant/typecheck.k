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
*    Main type checking phase.
******************************************************************************)
structure TypeCheck =
struct
   
   fun check prog =
      List.map (fn p =>
      let
         (** For a single declaration, generate a set of constraints
             and an updated value environment, as well as a term
             with type variables inserted in the appropriate places. *)
         val p' = Constrain.constrain p
         val _ = TyConstraints.constraints := 
                     Unify.unify (!TyConstraints.constraints)

         val _ = Debug.print Debug.internal ("Constraints:\n" ^ 
                                             TyConstraints.prettyPrint ()^"\n")

         val _ = TyConstraints.constraints := 
                  Unify.generalise 
                     (!Constrain.venv) 
                        (TyConstraints.getConstraints ())

         val _ = Debug.print Debug.internal ("Generalised Constraints:\n" ^ 
                                             TyConstraints.prettyPrint ()^"\n")

         val _ = TyConstraints.constraints := 
                     Unify.unify (!TyConstraints.constraints)

         val _ = Debug.print Debug.internal ("Solved Constraints:\n" ^ 
                                             TyConstraints.prettyPrint ()^"\n")
         val p'' = Unify.apply (TyConstraints.getConstraints ()) p'
         val _ = Constrain.apply (TyConstraints.getConstraints ())

         val _ = TyConstraints.clearConstraints ()
      in
         p''
      end) prog
end

