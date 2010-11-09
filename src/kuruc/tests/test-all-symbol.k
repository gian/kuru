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
*    Symbol table definition. 
*
*  Partially derived from http://www.cs.princeton.edu/~appel/modern/ml/chap4/
******************************************************************************)

structure Symbol =
struct
  type symbol = string * int

  fun name(s,n) = s
  fun hash(s,n) = n

  val toString = name

  val fromString = symbol

  fun enter (t,_,_) = t
  fun look _ = NONE
  fun keys _ = []

  fun symbol s = (s,0)

end

