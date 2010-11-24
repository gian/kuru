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

   val venv = ref S.empty : Temp.temp S.table ref
   val addEnv s t = 
      (venv := S.enter (!venv, s, t); t)

   fun tr_exp (I.N (I.INT i, _, _, _, _)) = (A.CONST i, [])
     | tr_exp (I.N (I.CHAR c, _, _, _, _)) = (A.CONST (Char.ord c), [])
     | tr_exp (I.N (I.STRING s, _, _, _, p)) = 
         Error.bug ("tr_exp", "Strings not implemented.", p)
     | tr_exp (I.N (I.REAL r, _, _, _, _)) = (A.RCONST r, [])
     | tr_exp (I.N (I.BOOL b, _, _, _, _)) = (A.BCONST b, [])
     | tr_exp (I.N (I.UNIT, _, _, _, _)) = (A.TEMP (Temp.newtemp Temp.unitt),[])
     | tr_exp (I.N (I.IDENT s, _, t, _)) =
         (* FIXME: this needs to do a lookup first! *)
         (A.TEMP (addEnv s (Temp.newtemp (tr_ty t))),[])
     | tr_exp (I.N (I.ENV s, _, t, _)) = (A.ENV (s, tr_ty t), [])
     | tr_exp (I.N (I.TUPLE, c, T.TUPLE t, _, _)) =
      let
         val tty = Temp.aggr (map tr_ty t)
         val t' = A.TEMP (Temp.newtemp (Temp.ptr tty))

         val offset = ref 0

         val ins = A.MOVE (t', A.MALLOC (tty,1)) ::
            List.foldl (
               fn (e, i) =>
               let
                  val (e',i) = tr_exp e
                  val elemty = tr_ty (I.getType e)
                  val tmp = I.TEMP (Temp.newtemp (Temp.ptr elemty))
                  val ind = !offset
                  val _ = offset := !offset + 1
               in
                  i @
                     [A.MOVE (tmp, A.ELEM_PTR (Temp.ptr tty, t', [0,ind])),
                      A.STORE (e',tmp)]
               end)
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
         [A.JUMP (I.NAME endLabel, [endLabel]),
          A.LABEL falseLabel] @
          ci @
         [A.JUMP (I.NAME endLabel, [endLabel]),
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
         val sources = ref [] : (exp * Temp.label) list ref

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
                      [I.LABEL expLabel] @
                      expi @
                      [I.JUMP (I.NAME endLabel, [endLabel])])
                  end
                | (I.N (_, _, _, _, p)) =>
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

   fun translate ir = []
end
