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
*  Turn functions containing pattern matches into case or let statements
*  containing a smaller set of pattern constructs, e.g.:
*  fn (a,b) => E --> fn x => let val a = #1 x; val b = #2 x in E end
*  fn 0 => E1 | 1 => E2 --> fn y => case y of 0 => E1 | 1 => E2 | _ => MatchExn
******************************************************************************)

structure AstMatchCompile =
struct
   fun apply tenv venv p = (tenv,venv,p)
end

