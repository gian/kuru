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
*    Translate TypedASTs to Intermediate Representation trees.
******************************************************************************)
structure Translate =
struct
	structure S = Symbol
	structure T = Type
	structure E = Env
	structure A = TypedAST
   structure I = Intermediate

   datatype fragment = 
      FSTRING of Temp.label * string


   val fragments = ref [] : fragment list ref

   fun add_fragment f = fragments := !fragments @ [f]
   fun add_fragments f = fragments := !fragments @ f
   fun reset_fragments () = fragments := []

   fun tr_ty _ = Temp.i32 (* FIXME *)

   fun tr_val (A.LINT (v,p)) = I.CONST v (* FIXME: negative numbers? *) 
     | tr_val (A.LREAL (v,p)) = I.RCONST v
     | tr_val (A.LCHAR (v,p)) = I.CONST (Char.ord v) 
     | tr_val (A.LSTRING (v,p)) =
       let
          val n = Temp.newlabel ()
          val _ = add_fragment (FSTRING (n, v))
       in
          I.NAME n
       end
     | tr_val (A.LUNIT p) = I.VOID 

   fun tr_pat ve se (A.PCONST (A.ELIT (v,t,_),_,_)) =
      (fn (c,t,f) =>
       let
          val v' = tr_val v
          val v'' =
          case v of 
            A.LINT _ => ( 
               [I.CJUMP2 (I.EQ,c,v',t,f)])
          | A.LREAL _ => raise Fail ("tr_pat: Invalid real pattern")
          | A.LCHAR _ => (
               [I.CJUMP2 (I.EQ,c,v',t,f)])
          | A.LSTRING _ => (
               let
                  val k = Temp.newtemp (Temp.i1)
               in
                  [I.MOVE (I.NAME k,I.kstreq c v'),
                   I.CJUMP2 (I.NEQ,
                            I.NAME k,
                            I.CONST 1,
                            t,
                            f)]
               end)
          | A.LUNIT _ => ([I.JUMP (I.NAME t,[t])])
       in
          (ve,se,v'')
       end)
     | tr_pat ve se (A.PCONST (_,_,p)) = 
            raise Fail ("tr_pat: Invalid constant pattern!")
     | tr_pat ve se (A.PLIST (v,t,p)) = 
            raise Fail ("tr_pat: List pat not implemented") 
     | tr_pat ve se (A.PTUPLE (v,t,p)) =
       (fn (c,t,f) =>
         let
            fun mkt (c,t,f) =  
               Vector.foldli (fn (i,e,(ve,se,l)) =>
               let
                  val nlab = Temp.newlabel ()
                  val f' = tr_pat ve se e
                  val (ve',se',l') = (f' (I.TUPLE_IND (c,i),nlab,f)) 
               in
                  (ve',se',l@[I.LABEL nlab]@l')
               end) (ve,se,[]) v

            val (ve',se',fl) = mkt (c,t,f) 
         in
            (ve',se',fl @ [I.JUMP (I.NAME t,[t])])
         end)
     | tr_pat ve se (A.PCONSTR (s,popt,t,p)) = 
            raise Fail ("tr_pat: PCONSTR not impl.")
     | tr_pat ve se (A.PID (s,ty,p)) =
       (fn (c,t,f) =>
       let
          val ts = Temp.newtemp (tr_ty ty)
          val ve' = S.enter (ve, s,ts)
       in
         (ve',se,
            [I.MOVE (I.NAME ts, c),
             I.JUMP (I.NAME t,[t])])
       end)
     | tr_pat ve se (A.PINFIX (a,b,c,t,p)) = 
            raise Fail "tr_pat: Infix not impl." 
(*
   and      texp = ELIT of tval * T.ty * pos
	              | EID of S.symbol * T.ty * pos
					  | ELID of S.symbol list * T.ty * pos
                 | ELIST of texp vector * T.ty * pos
                 | ETUPLE of texp vector * T.ty * pos
                 | EIF of texp * texp * texp * T.ty * pos
                 | EWHILE of texp * texp * T.ty * pos
                 | EFN of S.symbol * texp * T.ty * pos
                 | ECASE of texp * (tpat * texp) vector * T.ty * pos
                 | EAPP of texp * texp * T.ty * pos
                 | EEXN of texp * T.ty * pos
                 | ERAISE of texp * pos
                 | EHANDLE of tpat * texp * T.ty * pos
                 | ESEQ of texp vector * T.ty * pos
                 | ELET of tast vector * texp * T.ty * pos
                 | EANN of texp * T.ty * pos
                 
   and      tast = DVAL of S.symbol * texp * T.ty * pos
                 | DVALREC of tast * T.ty * pos
                 | DMUTUAL of tast vector * pos
                 | DSTRUCTURE of S.symbol * tast vector * pos
                 | DSTRUCTASSIGN of S.symbol * S.symbol * pos
                 | DNULL of pos *)

end
