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
*    A typed AST into which ParseTree trees are transformed.
******************************************************************************)
structure TypedAST =
struct
	structure T = Type
   structure S = Symbol

	type pos = ParseTree.pos

   datatype tval = LINT of int * pos
                 | LREAL of real * pos
                 | LCHAR of char * pos
                 | LSTRING of string * pos
                 | LUNIT of pos

   datatype tpat = PCONST of tval * T.ty * pos
                 | PLIST of tpat vector * T.ty * pos
                 | PTUPLE of tpat vector * T.ty * pos
                 | PCONSTR of S.symbol * tpat option * T.ty * pos
                 | PID of S.symbol * T.ty * pos 

   datatype texp = ELIT of tval * T.ty * pos
	              | EID of S.symbol * T.ty * pos
                 | ELIST of texp vector * T.ty * pos
                 | ETUPLE of texp vector * T.ty * pos
                 | EIF of texp * texp * texp * T.ty * pos
                 | EFN of tpat * texp * T.ty * pos
                 | ECASE of texp * (tpat * texp) * T.ty * pos
                 | EAPP of texp * texp * T.ty * pos
                 | EEXN of texp * T.ty * pos
                 | ERAISE of texp * pos
                 | EHANDLE of tpat * texp * T.ty * pos
                 
   datatype tast = DVAL of S.symbol * texp * T.ty * pos
                 | DMUTUAL of tast list * pos
end


