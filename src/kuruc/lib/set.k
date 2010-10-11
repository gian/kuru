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
*    A utility set type.
*  Loosely based upon:
*    http://823-tiger-compiler.googlecode.com/svn/trunk/milestone3/proj-base9/
******************************************************************************)

structure Set = struct
   type ''a set = ''a list 

   val empty = []
   fun all p sh = List.all p sh;
   fun delete([], _)    = []
     | delete(e::sh, y) = if e=y then sh else e::delete(sh,y)
   fun member(e, sl) = List.exists (fn y => e = y) sl
   fun inter(sh, sl) = List.filter (fn e => member(e, sl)) sh
   fun diff(sh, sl)  = List.filter (fn e => not (member(e, sl))) sh
   fun insert(e,sl) = if member(e,sl) then sl else e::sl
   fun union(sh, sl) = List.foldl insert sl sh
   fun subset(sh, sl) = List.all (fn e => member(e, sl)) sh
   fun size sh = List.length sh
   fun fromList sh = List.foldl insert empty sh
   fun toList sh = sh
   fun filter p sh = List.filter p sh
   fun exists p sh = List.exists p sh
   fun find p sh = List.find p sh
   fun map f s = List.foldl (fn (y,sl) => insert(f y, sl)) empty s
   fun fold f = List.foldl f
   fun split [] = NONE 
     | split (e::sh) = SOME(e,sh)
   fun singleton a = [a]
end

