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

   datatype structentry = StructEntry of I.exp S.table * structentry S.table

   datatype fragment = 
      FSTRING of Temp.label * string
    | FFN of Temp.label * I.exp list * I.stm list * Temp.ty


   val fragments = ref [] : fragment list ref

   fun add_fragment f = fragments := !fragments @ [f]
   fun add_fragments f = fragments := !fragments @ f
   fun reset_fragments () = fragments := []

   fun printFragment (FSTRING (l,s)) = 
      Temp.prettyPrint l ^ ": \"" ^ s ^ "\""
     | printFragment (FFN (l,el,sl,t)) =
      "function " ^ Temp.prettyPrint l ^ " (" ^
         String.concatWith "," (map I.ppe el) ^
       ") {\n   " ^
         String.concatWith "\n   " (map I.ppstm sl) ^ "\n}\n"

   fun printFragments [] = ""
     | printFragments (h::t) = printFragment h ^ "\n" ^ 
                                 printFragments t

   fun is_simple (I.NAME _) = true
     | is_simple (I.TEMP _) = true
     | is_simple (I.CONST _) = true
     | is_simple (I.BCONST _) = true
     | is_simple _ = false

   (* "Contingent label" - used to prevent adding of labels 
      to create empty blocks. *)
   fun cont_lab ([],_) = []
     | cont_lab (x,l) = l

   fun tr_ty (T.NAME s) = 
      let
         val s' = Symbol.name s
      in
         (case s' of 
            "int" => Temp.i32
          | "real" => Temp.r64
          | "char" => Temp.i32
          | "string" => Temp.ptr (Temp.i8)
          | "bool" => Temp.i1
          | "unit" => Temp.void
          | _ => raise Fail "tr_ty: unhandled NAME type.")
      end
     | tr_ty (T.FN (a,b)) =
         Temp.fn_ptr (tr_ty a, tr_ty b)
     | tr_ty (T.CONSTR (tl,t)) = raise Fail "tr_ty: CONSTR not impl."
     | tr_ty (T.TUPLE l) = Temp.aggr (map tr_ty l)
     | tr_ty _ = raise Fail "tr_ty: unhandled type"

   fun tr_val (A.LINT (v,p)) =   (I.CONST v,[]) (* FIXME: negative numbers? *) 
     | tr_val (A.LREAL (v,p)) =  (I.RCONST v,[])
     | tr_val (A.LCHAR (v,p)) =  (I.CONST (Char.ord v),[])
     | tr_val (A.LSTRING (v,p)) =
       let
          val n = Temp.newglobal ()
          val _ = add_fragment (FSTRING (n, v))
          val m = I.ELEM_PTR (Temp.array (size v + 1, Temp.i8),I.NAME n,0)
          val t = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
       in
          (t, [I.MOVE (t,m)])
       end
     | tr_val (A.LUNIT p) = (I.VOID,[]) 

   fun tr_pat ve se (A.PCONST (A.ELIT (v,t,_),_,_)) =
      (fn (c,t,f) =>
       let
          val (v',ins) = tr_val v
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
          (ve,se,ins@v'')
       end)
     | tr_pat ve se (A.PCONST (_,_,p)) = 
            raise Fail ("tr_pat: Invalid constant pattern!")
     | tr_pat ve se (A.PLIST (v,t,p)) = 
            raise Fail ("tr_pat: List pat not implemented") 
     | tr_pat ve se (A.PTUPLE (v,T.TUPLE types,p)) =
       (fn (c,t,f) =>
         let
            fun mkt (c,t,f) =  
               Vector.foldli (fn (i,e,(ve,se,l)) =>
               let
                  val nlab = Temp.newlabel ()
                  val f' = tr_pat ve se e
                  val rt = Temp.newtemp (tr_ty (List.nth (types,i)))
                  val l' = l @ [I.MOVE (I.TEMP rt,I.TUPLE_IND(c,i))]
                  val (ve',se',l'') = (f' (I.TEMP rt,nlab,f))
               in
                  (ve',se',l'@l''@[I.LABEL nlab])
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
          val ts = if is_simple c then c else I.TEMP (Temp.newtemp (tr_ty ty))
          val ve' = S.enter (ve, s, ts)
          val mv = if is_simple c then [] else [I.MOVE (ts,c)]
       in
         (ve',se,
            mv @ [I.JUMP (I.NAME t,[t])])
       end)
     | tr_pat ve se (A.PINFIX (a,b,c,t,p)) = 
            raise Fail "tr_pat: Infix not impl." 

   and tr_exp ve se (A.ELIT (v,t,p)) = 
       let 
          val (v',ins) = tr_val v
       in
          (ve,se,v',ins)
       end
	  | tr_exp ve se (A.EID (s,t,p)) = 
         (case S.look (ve,s) of NONE => raise Fail ("Unknown EID in tr_exp")
                              | SOME tm => (ve,se,tm,[]))
	  | tr_exp ve se (A.ELID (l,t,p)) = 
       let
          fun res vtbl stbl [] = raise Fail "tr_exp: BUG: empty long id?"
            | res vtbl stbl [id] = (case S.look (vtbl,id) of
                               NONE => raise Fail "tr_exp: BUG: unknown id?" 
                             | SOME k => k)
            | res vtbl stbl (h::rest) = 
                                   (case S.look (stbl,h) of
                               NONE => 
                                 raise Fail "tr_exp: BUG: unknown str id?"
                             | SOME (StructEntry (ve',se')) =>
                                       res ve' se' rest)
          val t' = res ve se l
       in
          (ve,se,t',[]) 
       end
     | tr_exp ve se (A.ELIST (v,t,p)) = raise Fail "tr_exp list not impl." 
     | tr_exp ve se (A.ETUPLE (v,T.TUPLE t,p)) = 
       let
          val (instr,rt) = 
            Vector.foldli (fn (i,a,(c',rt)) =>
               let
                  val (ve',se',a',c) = tr_exp ve se a
                  val rt' = Temp.newtemp (tr_ty (T.TUPLE t))
                  val ins = [I.MOVE(I.TEMP rt', I.TUPLE_INS (rt,a',i))]
               in
                  (c' @ c @ ins,I.TEMP rt')
               end) ([],I.UNDEF (tr_ty (T.TUPLE t))) v
       in
          (ve,se,rt,instr) 
       end
     | tr_exp ve se (A.EIF (e1,e2,e3,t,p)) =
       let
          val (_,_,r1,in1) = tr_exp ve se e1

          val tlab = Temp.newlabel ()
          val (_,_,r2,in2) = tr_exp ve se e2

          val flab = Temp.newlabel ()
          val (_,_,r3,in3) = tr_exp ve se e3

          val rlab = Temp.newlabel ()

          val rv = I.TEMP (Temp.newtemp (tr_ty t))
          val phi = [I.LABEL rlab, I.PHI (rv,tr_ty t,[(r2,tlab),(r3,flab)])]
       in
          (ve,se,rv,
          in1 @
          [I.CJUMP (r1, tlab, flab),
           I.LABEL tlab] @
          in2 @
          [I.JUMP (I.NAME rlab, [rlab]),
           I.LABEL flab] @
          in3 @
          [I.JUMP (I.NAME rlab, [rlab])] @
          phi)
       end
     | tr_exp ve se (A.EWHILE (c,e,t,p)) = raise Fail "tr_exp while not impl." 
     | tr_exp ve se (A.EFN (s,e,T.FN(it,rt),p)) =
       let
          val fid = Temp.newglobal ()
          val p = Temp.newtemp (tr_ty it)
          val ve' = S.enter (ve,s,I.TEMP p)
          val (ve'',se',e',instr) = tr_exp ve' se e
          val _ = add_fragment (FFN (fid,[I.TEMP p],
                                            [I.LABEL (Temp.newlabel ())] @
                                                   instr @ 
                                                   [I.RET e'],tr_ty rt))
       in
          (ve,se,I.NAME fid, [])
       end
     | tr_exp ve se (A.ECASE (e,v,t,p)) =
       let
          val (_,_,e',in1) = tr_exp ve se e

          val el = Temp.newlabel ()
          
          val (instr,labs) =
            Vector.foldl (
               fn ((p,e),(ins,labs)) =>
               let
                  val tlab = Temp.newlabel ()
                  val flab = Temp.newlabel ()
                  val (ve',se',p') = (tr_pat ve se p) (e',tlab,flab)
                  val (_,_,e'',eins) = tr_exp ve' se' e
               in
                  (ins @ 
                    p' @ 
                    [I.LABEL tlab] @ 
                    eins @
                    [I.JUMP (I.NAME el, [el]),
                     I.LABEL flab],
                  labs @ [(e'',tlab)])
               end) ([],[]) v

          val rt = Temp.newtemp (tr_ty t)
          val instr' = in1 @
                       instr @ 
                        [I.LABEL el, 
                         I.PHI (I.TEMP rt, tr_ty t, labs)]
       in
          (ve,se,I.TEMP rt,instr')
       end
     | tr_exp ve se (A.EAPP (a,b,t,p)) = 
       let
          val (_,_,b',instr1) = tr_exp ve se b
          val (_,_,a',instr2) = tr_exp ve se a
          val rt = Temp.newtemp (tr_ty t)
          val instr3 = [
                 I.MOVE (I.TEMP rt,
                         I.CALL (tr_ty t,a',b'))
              ]
       in
          (ve,se,I.TEMP rt, instr1 @ instr2 @ instr3)
       end
     | tr_exp ve se (A.EEXN (e,t,p)) = raise Fail "tr_exp: EEXN not impl." 
     | tr_exp ve se (A.ERAISE (e,p)) = raise Fail "tr_exp: ERAISE not impl." 
     | tr_exp ve se (A.EHANDLE (pa,e,t,p)) = raise Fail "tr_exp: EHANDLE"
     | tr_exp ve se (A.ESEQ (v,t,p)) =
       let
          val (rt,v') =
            Vector.foldl (
                fn (e,(rt,v)) =>
                  let
                     val (_,_,rt',instr) = tr_exp ve se e
                  in
                     (rt',v@instr)
                  end
            ) (I.TEMP (Temp.newtemp (tr_ty t)),[]) v
       in
          (ve,se,rt,v')
       end
     | tr_exp ve se (A.ELET _) = raise Fail "tr_exp: ELET remains!" 
     | tr_exp ve se (A.EANN (e,t,p)) = tr_exp ve se e 
                 
   and tr_ast ve se (A.DVAL (s,e,t,p)) =
       let
          val _ = Debug.print Debug.debug ("tr_ast: DVAL\n")
          val (_,_,e',in1) = tr_exp ve se e
          
          val rt = if is_simple e' then e' else I.TEMP (Temp.newtemp (tr_ty t))
          val ve' = S.enter (ve,s,rt)
          val mv = if is_simple e' then [] else [I.MOVE (rt,e')]
       in
          (ve',se,in1 @ mv)
       end
     | tr_ast ve se (A.DVALREC (A.DVAL (s,e,t1,p1),t2,p2)) =
       let
          val rt = Temp.newtemp (tr_ty t1)
          val ve' = S.enter (ve,s,I.TEMP rt)
          val (_,_,e',in1) = tr_exp ve' se e
       in
          (ve',se,in1 @ [I.MOVE (I.TEMP rt, e')])
       end
     | tr_ast ve se (A.DVALREC (a,t,p)) =
         tr_ast ve se a
     | tr_ast ve se (A.DMUTUAL (v,p)) = 
         Vector.foldl (fn (e,(ve,se,b)) =>
            let
               val (ve',se',b') = tr_ast ve se e
            in
               (ve',se',b@b')
            end) (ve,se,[]) v 
     | tr_ast ve se (A.DSTRUCTURE (s,v,p)) = 
       let
          val (ve',se',instr) =
            Vector.foldl (fn (e,(ve,se,b)) =>
            let
               val (ve',se',b') = tr_ast ve se e
            in
               (ve',se',b@b')
            end) (ve,se,[]) v 
          val se'' = S.enter (se,s,StructEntry (ve',se'))
       in
          (ve,se'',instr)
       end
     | tr_ast ve se (A.DSTRUCTASSIGN (s1,s2,p)) =
         raise Fail "tr_ast: DSTRUCTASSIGN not impl."
         (* FIXME: DSTRUCTASSIGN needs to accept long ids on RHS. *)
     | tr_ast ve se (A.DNULL p) = (ve,se,[]) 

   val tr_venv_base =
      let
         val b = S.empty
         val b' = S.enter (b, S.symbol "true", I.BCONST true)
         val b'' = S.enter (b', S.symbol "false", I.BCONST false)
      in
         b''
      end

   fun translate ast =
      let
         val (ve,se,ast') =
            List.foldl (fn (a,(ve,se,b)) =>
                    let
                       val (ve',se',a') = tr_ast ve se a
                    in
                       (ve',se',b@a')
                    end) (tr_venv_base,S.empty,[]) ast
         val fr = !fragments
         val _ = fragments := []
      in
         (ve,se,ast',fr)
      end
end
