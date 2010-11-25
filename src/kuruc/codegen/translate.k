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
	structure I = Intermediate
   structure A = Asm

   val venv = ref S.empty : A.exp S.table ref
   
   fun addEnv s t = 
      (venv := S.enter (!venv, s, t); t)
   
   fun getEnv p s =
      case S.look (!venv,s) of
         NONE => Error.bug ("Translate.getEnv",
                     "Cannot find symbol '" ^ S.name s ^ "'", p)
       | SOME x => x

   fun tr_ty (T.NAME s) =
      (case S.name s of
         "int" => Temp.i32
       | "real" => Temp.r32
       | "char" => Temp.i8
       | "bool" => Temp.i1
       | "void" => Temp.void
       | n => Temp.named n)
     | tr_ty (T.FN (t1,t2)) =
         Temp.fn_ptr (map tr_ty t1, tr_ty t2)
     | tr_ty (T.TUPLE t) = Temp.aggr (map tr_ty t)
     | tr_ty (T.CONSTR ([t1],t2)) = tr_ty t2 (* FIXME *)
     | tr_ty t = 
      Error.bug ("Translate.tr_ty", "Not implemented: " ^ T.prettyPrint t, (0,""))

   fun tr_pat ex (I.N (I.INT i,_,_,_,_)) =
      let
         val rt = A.TEMP (Temp.newtemp Temp.i1)
      in
         (rt, [A.MOVE (rt, A.CMP (A.EQ, A.CONST i, ex))])
      end
     | tr_pat ex (I.N (I.CHAR c,_,_,_,_)) =
      let
         val rt = A.TEMP (Temp.newtemp Temp.i1)
      in
         (rt, [A.MOVE (rt, A.CMP (A.EQ, A.CONST (Char.ord c), ex))])
      end
     | tr_pat ex (I.N (I.STRING i,_,_,_,_)) =
      let
         val rt = A.BCONST false
         val _ = Debug.print Debug.warning "String patterns not implemented!\n"
      in
         (rt, [])
      end
     | tr_pat ex (I.N (I.REAL _,_,_,_,p)) =
         Error.bug ("Translate.tr_pat", "Invalid real constant pattern", p)
     | tr_pat ex (I.N (I.BOOL b,_,_,_,_)) =
      let
         val rt = A.TEMP (Temp.newtemp Temp.i1)
      in
         (rt, [A.MOVE (rt, A.BINOP ("and", A.BCONST b, ex))])
      end
     | tr_pat ex (I.N (I.UNIT, _, _, _, _)) =
      let
         (* For well-typed programs, this is always true. *)
         val rt = A.BCONST true
      in
         (rt,[])
      end
     | tr_pat ex (I.N (I.IDENT s,_,_,_,_)) =
      let
         val _ = addEnv s ex
         (* FIXME: is this right? Do we need to roll back if the
            larger match (e.g. the rest of the tuple) fails? 
            Probably not, as names are unique, so at worst we have
            bound a name that is not referenced anywhere. *)
      in
         (A.BCONST true, [])
      end
     | tr_pat ex (I.N (I.TUPLE, v, T.TUPLE t, _, _)) =
      let
         val tty = Temp.aggr (map tr_ty t)

         (* The layout looks like this:
            entryLabel:
               %el0 = extract 0 %ex
               <tr_pat el0 pat0>
               br i1 %pat0 %el1Label, %endLabel
            el1Label:
               %el1 = extract 1 %ex
               <tr_pat el1 pat1>
               br i1 %pat1 %el2Label, %endLabel
            el2Label:
               ...
            elNLabel:
               br label sucLabel
            sucLabel:
               br label valueLabel
            endLabel:
               br label valueLabel
            valueLabel:
               %rt = phi [true %sucLabel, false %endLabel]
         *)

         val entryLabel = Temp.newlabel ()
         val sucLabel   = Temp.newlabel ()
         val endLabel   = Temp.newlabel ()
         val valueLabel = Temp.newlabel ()
         val rt = A.TEMP (Temp.newtemp Temp.i1)

         val vindex = ref 0

         val body = List.concat
            (List.map
               (fn v' =>
               let
                  val t' = tr_ty (I.getType v')
                  val el = A.TEMP (Temp.newtemp (Temp.ptr t'))
                  val el' = A.TEMP (Temp.newtemp t')
                  val nxtLabel = Temp.newlabel ()
                  val (pat,pati) = tr_pat el' v'
                  val ix = !vindex
                  val _ = vindex := !vindex + 1
               in
                  [A.MOVE (el,A.ELEM_PTR (Temp.ptr tty, ex, [0,ix])),
                   A.MOVE (el',A.LOAD el)] @
                  pati @
                  [A.CJUMP (pat, nxtLabel, endLabel),
                   A.LABEL nxtLabel]
               end)
            v)
      in
         (rt,
         [A.LABEL entryLabel] @
         body @
         [A.JUMP (A.NAME sucLabel, [sucLabel]),
          A.LABEL sucLabel,
          A.JUMP (A.NAME valueLabel, [valueLabel]),
          A.LABEL endLabel,
          A.JUMP (A.NAME valueLabel, [valueLabel]),
          A.LABEL valueLabel,
          A.PHI (rt, Temp.i1, [(A.BCONST true, sucLabel),
                               (A.BCONST false, endLabel)])])
      end
     | tr_pat ex (I.N (I.PWILD, _, _, _, _)) = (A.BCONST true, [])
     | tr_pat ex (I.N (I.NIL, _, _, _, _)) =
      let
          val rt = A.TEMP (Temp.newtemp Temp.i1)
      in
          (rt, [A.MOVE (rt, A.CMP (A.EQ, A.NULL (Temp.ptr Temp.i8), ex))])
      end
     | tr_pat ex (x as I.N (_,_,_,_,p)) =
         Error.bug ("Translate.tr_pat", "Invalid pattern: " ^ I.prettyPrint x, p)

   and tr_exp (I.N (I.INT i, _, _, _, _)) = (A.CONST i, [])
     | tr_exp (I.N (I.CHAR c, _, _, _, _)) = (A.CONST (Char.ord c), [])
     | tr_exp (I.N (I.STRING s, _, _, _, p)) = 
         Error.bug ("tr_exp", "Strings not implemented.", p)
     | tr_exp (I.N (I.REAL r, _, _, _, _)) = (A.RCONST r, [])
     | tr_exp (I.N (I.BOOL b, _, _, _, _)) = (A.BCONST b, [])
     | tr_exp (I.N (I.UNIT, _, _, _, _)) = 
         (A.TEMP (Temp.newtemp (Temp.named "unit")),[])
     | tr_exp (I.N (I.IDENT s, _, t, _, p)) = (getEnv p s, [])
     | tr_exp (I.N (I.ENV s, _, t, _, _)) = (A.ENV (s, tr_ty t), [])
     | tr_exp (I.N (I.TUPLE, c, T.TUPLE t, _, _)) =
      let
         val tty = Temp.aggr (map tr_ty t)
         val t' = A.TEMP (Temp.newtemp (Temp.ptr tty))

         val offset = ref 0

         val ins = A.MOVE (t', A.MALLOC (tty,1)) ::
            (List.foldl (
               fn (e, i) =>
               let
                  val (e',i) = tr_exp e
                  val elemty = tr_ty (I.getType e)
                  val tmp = A.TEMP (Temp.newtemp (Temp.ptr elemty))
                  val ind = !offset
                  val _ = offset := !offset + 1
               in
                  i @
                     [A.MOVE (tmp, A.ELEM_PTR (Temp.ptr tty, t', [0,ind])),
                      A.STORE (e',tmp)]
               end)) [] c
      in
         (t',ins)
      end
     | tr_exp (I.N (I.IF, [a,b,c], t, _, p)) =
      let
         (* Layout will look like this:
            entryLabel:
               <code to calculate %a>
               br i1 %a, %trueLabel, %falseLabel
            trueLabel:
               <code to calculate %b>
               br %endLabel
            falseLabel:
               <code to calculate %c>
               br %endLabel
            endLabel:
               %res = phi [%b trueLabel, %c falseLabel]

            However, if either of the true or false codeblocks are empty,
            we have to set the trueSource/falseSource to the entry block
            instead and elipse the entire block!
         *)

         val entryLabel = Temp.newlabel ()
         val trueLabel  = Temp.newlabel ()
         val falseLabel = Temp.newlabel ()
         val endLabel   = Temp.newlabel ()

         val (a',ai) = tr_exp a
         val (b',bi) = tr_exp b
         val (c',ci) = tr_exp c

         val (trueSource,trueDest) = 
               if length bi = 0 then (entryLabel,endLabel) 
                                else (trueLabel,trueLabel)
         val (falseSource,falseDest) = 
               if length ci = 0 then (entryLabel,endLabel)
                                else (falseLabel,falseLabel)

         val rt = A.TEMP (Temp.newtemp (tr_ty t))
      in
         (rt, 
          A.LABEL entryLabel ::
          ai @
         [A.CJUMP (a',trueDest,falseDest),
          A.LABEL trueLabel] @
          bi @
         [A.JUMP (A.NAME endLabel, [endLabel]),
          A.LABEL falseLabel] @
          ci @
         [A.JUMP (A.NAME endLabel, [endLabel]),
          A.LABEL endLabel,
          A.PHI (rt, tr_ty t, [(b',trueSource),(c',falseSource)])])
      end
     | tr_exp (I.N (I.WHILE, _, _, _, p)) = 
         Error.bug ("tr_exp", "While not implemented", p)
     | tr_exp (I.N (I.FN, _, _, _, p)) =
         Error.bug ("Translate.tr_exp", 
            "Function remains after closure conversion", p)
     | tr_exp (I.N (I.CASE, e::cases, t, _, p)) =
      let
         (* We build up the eventual PHI body. *)
         val sources = ref [] : (A.exp * Temp.label) list ref

         val entryLabel = Temp.newlabel ()
         val (e',ei) = tr_exp e
         val rt = A.TEMP (Temp.newtemp (tr_ty t))
         val endLabel = Temp.newlabel ()

         (* The layout is this:
            entryLabel:
               <code for %e>
               <code for %p1>
               br i1 %p1, %exp1Label, %nextLabel
            nextLabel:
               <code for %p2>
               br i1 %p2, %exp2Label, %nextLabel2
            nextLabel2:
               (Match exception)
               unwind
            exp1Label:
               <code for %exp1>
               br label %endLabel
            exp2Label:
               <code for %exp2>
               br label %endLabel
            endLabel:
               %rt = phi [%exp1 exp1Label, %exp2 exp2Label] *)

         val (pats,exps) =
            List.foldl (
               fn (I.N (I.MATCH, [pat,exp], _, _, _),(pats,exps)) =>
                  let
                     val (pat',pati) = tr_pat e' pat
                     val (exp',expi) = tr_exp exp
                     val expLabel = Temp.newlabel ()
                     val nextLabel = Temp.newlabel ()
                     val _ = sources := !sources @ [(exp',expLabel)]
                  in
                     (pats @ 
                      pati @
                      [A.CJUMP (pat',expLabel,nextLabel),
                       A.LABEL nextLabel],
                      exps @
                      [A.LABEL expLabel] @
                      expi @
                      [A.JUMP (A.NAME endLabel, [endLabel])])
                  end
                | (I.N (_, _, _, _, p),_) =>
                     Error.bug ("Translate.tr_exp", "Invalid non-match", p))
               ([],[]) cases
         
      in
         (rt,
          ei @
          pats @
         [A.UNWIND] @
          exps @
         [A.LABEL endLabel,
          A.PHI (rt, tr_ty t, !sources)])
      end
     | tr_exp (I.N (I.MATCH, _, _, _, p)) = 
         Error.bug ("Translate.tr_exp", "Unattached MATCH node", p)
     | tr_exp (I.N (I.APP, [a, b], t, _, p)) =
      let
         val t' = tr_ty t

         val (a',ai) = tr_exp a
         val (b',bi) = tr_exp b

         val rt = A.TEMP (Temp.newtemp t')
      in
         if t' = Temp.void then
            (A.VOID, 
             ai @ bi @ [A.EXP (A.CALL (t',a',b'))])
         else (rt, ai @ bi @ [A.MOVE (rt, A.CALL (t',a',b'))])
      end
     | tr_exp (I.N (I.RAISE, [e], t, _, p)) =
      let
         val (e',ei) = tr_exp e
      in
         (A.VOID,
          ei @ [A.EXP (A.CALL (Temp.void, A.BUILTIN "RAISE", e')),
                A.UNREACHABLE])
      end
     | tr_exp (I.N (I.HANDLE,_,_,_,p)) = 
         Error.bug ("Translate","Handle not implemented", p)
     | tr_exp (I.N (I.SEQ, c, t, _, p)) =
      let
         val rt = ref A.VOID

         fun et [] = Error.bug ("Translate.tr_exp","Sequence empty",p)
           | et [c] =
            let
               val (c',ci) = tr_exp c
               val _ = rt := c'
            in
               ci
            end
           | et (h::t) = ((fn (_,i) => i) (tr_exp h)) @ et t

         val ins = et c
      in
         (!rt,ins)
      end
     | tr_exp (I.N (I.LET, c, t, _, p)) =
      let
         val c' = List.rev c
         val e = hd c'
         val d = List.rev (tl c')
         val d' = List.concat (map tr_stm d)
         val (e',ei) = tr_exp e
      in
         (e',d' @ ei)
      end
     | tr_exp (I.N (I.CON i, c, t, _, p)) =
      let
         val t' = Temp.aggr [Temp.i32, Temp.ptr Temp.i8]
         val (c',ci) =
            case c of [] => (A.NULL (Temp.ptr Temp.i8), [])
                    | [c] => 
                        let
                           val (c',ci) = tr_exp c
                           val c'' = A.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
                        in
                           (c'', ci @
                            [A.MOVE (c'',A.BITCAST (c',Temp.ptr Temp.i8))])
                        end
                    | _ => Error.bug ("Translate.tr_exp",
                                      "Constructor with > 1 arg", p)

         val rt = A.TEMP (Temp.newtemp (Temp.ptr t'))
         val tmp1 = A.TEMP (Temp.newtemp (Temp.ptr Temp.i32))
         val tmp2 = A.TEMP (Temp.newtemp (Temp.ptr (Temp.ptr Temp.i8)))

         val ins = ci @
            [A.MOVE (rt, A.MALLOC (t',1)),
             A.MOVE (tmp1, A.ELEM_PTR (Temp.ptr t', rt, [0,0])),
             A.MOVE (tmp2, A.ELEM_PTR (Temp.ptr t', rt, [0,1])),
             A.STORE (A.CONST i, tmp1),
             A.STORE (c',tmp2)]
      in
         (rt,ins)
      end
     | tr_exp (I.N (I.CLOSURE, f::env, t, _, p)) =
      let
         val (f',fi) = tr_exp f
         val (env',envi) = ListPair.unzip (map tr_exp env)
         val ety = map (tr_ty o I.getType) env

         val cty = Temp.aggr ((tr_ty t) :: ety)
         val rt = A.TEMP (Temp.newtemp (Temp.ptr cty))
         val tmp1 = A.TEMP (Temp.newtemp (Temp.ptr (tr_ty t)))

         val ix = ref 0

         val ins = fi @ (List.concat envi) @
            [A.MOVE (rt, A.MALLOC (cty,1)),
             A.MOVE (tmp1, A.ELEM_PTR (Temp.ptr cty, rt, [0,0])),
             A.STORE (f', tmp1)] @
           (List.concat (List.map
            (fn (v,t) => 
             let
                val tmp = A.TEMP (Temp.newtemp t)
                val ix' = !ix
                val _ = ix := ix' + 1
             in
               [A.MOVE (tmp, A.ELEM_PTR (Temp.ptr cty, rt, [0,ix'])),
                A.STORE (v,tmp)]
             end)
            (ListPair.zip (env',ety))))
      in
         (rt,ins)
      end
     | tr_exp (x as I.N (_,_,_,_,p)) = 
      Error.bug ("Translate.tr_exp", "Not implemented: " ^ I.prettyPrint x,p)

   and tr_stm (I.N (I.BIND s, [e], t, _, p)) =
      let
         val (e',ei) = tr_exp e
         val _ = addEnv s e' 
      in
         ei
      end
     | tr_stm (I.N (I.MUTUAL, v, _, _, p)) = 
         List.concat (map tr_stm v)
     | tr_stm (I.N (I.IMPORT (ename, T.FN ([it],ot), iname), _, _, _, _)) =
      let
         val b = A.BUILTIN (S.name ename)
         val _ = addEnv iname b
      in
         [A.DECLARE (tr_ty ot, S.name ename, tr_ty it)]
      end
     | tr_stm (I.N (I.PROGRAM, v, _, _, p)) =
         List.concat (map tr_stm v)
     | tr_stm (s as I.N (_,_,_,_,p)) = 
         Error.bug ("Translate.tr_stm", "Unhandled: " ^ I.prettyPrint s, p)

   fun translate ir = tr_stm ir 
end
