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
*    Type environment entries
******************************************************************************)
structure Env =
struct
	structure T = Type
   structure S = Symbol

	datatype enventry = 
					  VarEntry of S.symbol * T.ty

   datatype structentry =
                 StructEntry of T.ty Symbol.table * 
                            enventry Symbol.table * 
                         structentry Symbol.table
               | SigEntry of T.ty Symbol.table *
                            enventry Symbol.table *
                         structentry Symbol.table
				   | ClassEntry of T.ty Symbol.table *
									 enventry Symbol.table *
								 structentry Symbol.table *
								 (T.ty * structentry) list ref
               | ParentEntry of structentry Symbol.table

	fun pp (VarEntry (v,t)) = T.prettyPrint t

	val prettyPrint = pp

	type env = enventry Symbol.table

	val tenv_base = 
      let
         val benv =
          [
            ("int",        T.t_int),
            ("string",     T.t_string),
            ("char",       T.t_char),
            ("real",       T.t_real),
            ("_opaque",    T.t_opaque),
            ("list",       T.t_list),
            ("cvoid",      T.t_void),
            ("unit",       T.t_unit),
            ("bool",       T.t_bool),
            ("exn",        T.t_exn)
          ]

         val b = 
            List.foldl (fn ((s,t),st) => 
               S.enter (st,  S.symbol s, t))
                  Symbol.empty benv
      in
         b
      end

   val venv_base = Symbol.empty : enventry Symbol.table

	val senv_base = 
      let
         val consty = T.POLY 0
         val listty = T.CONSTR ([consty],T.t_list)
         val benv =
          [
            ("=",           T.FN([T.TUPLE [T.fresh(),T.fresh()]],T.t_bool))
        (*    ("Match",       T.t_exn),
            ("+",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("-",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("*",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("/",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("<>",          T.FN([T.TUPLE [T.fresh(),T.fresh()]],T.t_bool)),
            ("<",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            (">",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            ("<=",          T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            (">=",          T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            ("andalso",    T.FN([T.TUPLE [T.t_bool,T.t_bool]],T.t_bool)),
            ("orelse",     T.FN([T.TUPLE [T.t_bool,T.t_bool]],T.t_bool)),
            ("print",       T.FN ([T.t_string], T.t_void)),
            ("toString",    T.FN ([T.t_int],T.t_string)), 
            ("::",          T.FN([T.TUPLE [consty,listty]],listty)),
            ("@",           T.FN([T.TUPLE [listty,listty]],listty)) *)
          ]
         val b = 
            List.foldl (fn ((s,t),st) => 
               S.enter (st,  S.symbol s, VarEntry (S.symbol s,t)))
                  Symbol.empty benv

         val b' = S.enter (Symbol.empty, S.symbol "Internal",
                              StructEntry (S.empty, b, S.empty))

         val b'' = S.enter (Symbol.empty, S.symbol "Compiler",
                              StructEntry (S.empty, S.empty, b'))

      in
         b''
      end
end

