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
*    Type constraint generation and solving.
******************************************************************************)
structure Constrain =
struct
	structure S = Symbol
	structure T = Type
	structure E = Env
	structure A = TypedAST

   val venv = ref S.empty : T.ty S.table ref

   fun setType s t = venv := S.enter (!venv,s,t)

   fun getType s = S.look (!venv,s)

   fun substinenv (tn,tr,p) =
      let
			val vkeys = S.keys (!venv) 
		
			fun upd NONE _ = 
				 Error.bug ("Constrain","substinenv updates unknown venv symbol",p)
			  | upd (SOME s) t = 
			  		venv := S.enter (!venv,s,t)

         fun updVal () =
            List.app (fn (s,t) =>
				   upd (S.unhash s) 
					   (T.substinty tn tr t)) vkeys
		in
		   updVal ()
      end

   fun apply [] = ()
     | apply ((l,r,p)::t) = 
         (substinenv (l,r,p); apply t)

   fun addOptConstraint (_,T.UNKNOWN,p) = ()
     | addOptConstraint _ = ()
   (*  | addOptConstraint (t1,t2,p) = TyConstraints.add (t1,t2,p) *)


   val consty = T.POLY 0
   val listty = T.CONSTR ([consty],T.t_list)

    val benv =
          [
            ("true",        T.t_bool),
            ("false",       T.t_bool),
            ("Match",       T.t_exn),
            ("+",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("-",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("*",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("/",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_int)),
            ("=",           T.FN([T.TUPLE [T.fresh(),T.fresh()]],T.t_bool)),
            ("<>",          T.FN([T.TUPLE [T.fresh(),T.fresh()]],T.t_bool)),
            ("<",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            (">",           T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            ("<=",          T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            (">=",          T.FN([T.TUPLE [T.t_int,T.t_int]],T.t_bool)),
            ("andalso",     T.FN([T.TUPLE [T.t_bool,T.t_bool]],T.t_bool)),
            ("orelse",      T.FN([T.TUPLE [T.t_bool,T.t_bool]],T.t_bool)),
            ("print",       T.FN ([T.t_string], T.t_void)),
            ("toString",    T.FN ([T.t_int],T.t_string)),
            ("::",          T.FN([T.TUPLE [consty,listty]],listty)),
            ("@",           T.FN([T.TUPLE [listty,listty]],listty))
          ]

   val _ = List.app (fn (a,b) => setType (S.symbol a) b) benv


   fun inst l =
      let
         val instMap = ref S.empty : T.ty S.table ref

         val _ = Debug.print Debug.debug (
                     "Instantiating: " ^ A.ppty l
                  )

         fun inst' (T.POLY i) = 
               (case S.look (!instMap,S.symbol (Int.toString i)) of
                  SOME t => t
                | NONE => 
                  let
                     val tx = T.fresh ()
                     val _ = instMap := S.enter 
                                          (!instMap,S.symbol (Int.toString i),tx)
                  in
                     tx
                  end)
           | inst' (T.FN (a,b)) = T.FN (map inst' a, inst' b)
           | inst' (T.TUPLE l) = T.TUPLE (map inst' l)
           | inst' (T.CONSTR (l,t)) = T.CONSTR (map inst' l, inst' t)
           | inst' (T.META x) = T.META x
           | inst' (T.NAME s) = T.NAME s
           | inst' t =
               Error.bug ("Constrain.inst",
                  "Instantiating unknown: " ^ T.prettyPrint t, (0,""))
      in
         inst' l
      end

   fun constrain de =
   let
      fun cval (A.LINT _) = T.t_int
        | cval (A.LREAL _) = T.t_real
        | cval (A.LCHAR _) = T.t_char
        | cval (A.LSTRING _) = T.t_string
        | cval (A.LUNIT _) = T.t_unit

      fun cpat (A.PCONST (e,t,p)) = 
      let
         val (te,e') = cexp e
         val _ = addOptConstraint (te,t,p)
      in
         (te,A.PCONST (e',te,p))
      end
        | cpat (A.PTUPLE (v,t,p)) =
      let
         val (t',v') = 
            Vector.foldl (
               fn (a,(t,v)) =>
               let
                  val (t',v') = cpat a
               in
                  (t@[t'],v@[v'])
               end) ([],[]) v

         val t'' = T.TUPLE t'
         val _ = addOptConstraint (t'',t,p)
      in
         (t'',A.PTUPLE (Vector.fromList v',t'',p))
      end
        | cpat (A.PCONSTR (i,NONE,t,p)) = (t,A.PCONSTR (i,NONE,t,p))
        | cpat (A.PCONSTR (i,SOME (e,t1),t2,p)) =
      let
         val (te,e') = cpat e
         val _ = TyConstraints.add (te,t1,p)
      in
         (t2,A.PCONSTR (i,SOME (e',te),t1,p))
      end
        | cpat (A.PID (s,t,p)) =
      let
         val tx = T.fresh ()
         val _ = setType s tx
         (* val _ = addOptConstraint (tx,t,p) *)
      in
         (tx,A.PID (s,tx,p))
      end
        | cpat (A.PINFIX (a,b,c,t,p)) =
      let
         (** @todo Add custom inverses *)
         val _ = 
            case S.name b of 
               "::" => ()
             | s => Error.fail ("Invalid infix operator '"^s^"' in pattern",p)

         (* Proceed under the assumption that operator is :: *)
         val (ta,a') = cpat a
         val (tc,c') = cpat c
         val tx = T.fresh ()
         val tt = T.CONSTR ([tx],T.t_list)
         val _ = TyConstraints.add (tx,ta,p)
         val _ = TyConstraints.add (tc,tt,p)
      in
         (tt, A.PINFIX (a',b,c',tt,p))
      end
        | cpat (A.PWILD p) = (T.fresh (), A.PWILD p)
        | cpat (A.PNIL p) = (T.CONSTR ([T.fresh()],T.t_list), A.PNIL p)
        | cpat (A.PUNIT p) = (T.t_unit, A.PUNIT p)
        | cpat x = Error.bug ("Constrain","Unhandled pattern: "^A.ppp x,(0,""))

      and cexp (A.ELIT (v,t,p)) = 
      let
         val t' = cval v
         val _ = addOptConstraint (t',t,p)
      in
         (t',A.ELIT (v,t',p))
      end
        | cexp (A.EID (s,t,p)) =
      let
         val t' = 
            case getType s of
               NONE =>
               let
                  val t' = T.fresh ()
                  val _ = setType s t'
               in
                  t'
               end
             | SOME t => t

         val _ = addOptConstraint (t',t,p)
      in
         (t',A.EID (s,t',p))
      end
        | cexp (A.ENIL p) = 
            (T.CONSTR ([T.fresh ()],T.t_list),A.ENIL p)
        | cexp (A.ETUPLE (v,t,p)) =
      let
         val (t',v') = ListPair.unzip 
            (Vector.foldl (fn (a,b) => b @ [(cexp a)]) [] v)

         val _ = addOptConstraint (T.TUPLE t',t,p)
      in
         (T.TUPLE t', A.ETUPLE (Vector.fromList v', T.TUPLE t', p))
      end
        | cexp (A.EIF (a,b,c,t,p)) =
      let
         val (ta,a') = cexp a
         val (tb,b') = cexp b
         val (tc,c') = cexp c
         val tx = T.fresh ()
         val _ = TyConstraints.add (tx,tb,p)
         val _ = TyConstraints.add (tx,tc,p)
         val _ = TyConstraints.add (ta,T.t_bool,p)
         val _ = addOptConstraint (tx,t,p)
      in
         (tx,A.EIF (a',b',c',tx,p))
      end
        | cexp (A.EWHILE (a,b,t,p)) =
      let
         val (ta,a') = cexp a
         val (tb,b') = cexp b
         val _ = TyConstraints.add (ta,T.t_bool,p)
         val tx = T.CONSTR ([tb],T.t_list)
         val _ = addOptConstraint (tx,t,p)
      in
         (tx,A.EWHILE (a',b',tx,p))
      end
        | cexp (A.EFN (s,e,t,p)) =
      let
         val ts = List.foldl (fn (s,ts) =>
            let
               val t' = T.fresh ()
               val _ = setType s t'
            in
               ts @ [t']
            end) [] s

         val (te,e') = cexp e
         val tx = T.FN (ts,te)
      in
         (tx,A.EFN (s,e',tx,p))
      end
        | cexp (A.ECASE (e,v,t,p)) = 
      let
         val ti = T.fresh ()
         val tx = T.fresh ()

         val (te,e') = cexp e

         val _ = TyConstraints.add (ti,te,p)

         val v' = Vector.map (
            fn (pat,exp) => 
               let
                  val (tp,p') = cpat pat
                  val (te,e') = cexp exp
                  val _ = TyConstraints.add (ti,tp,p)
                  val _ = TyConstraints.add (tx,te,p)
               in
                  (p',e')
               end) v

         val _ = addOptConstraint (tx,t,p)
      in
         (tx,A.ECASE (e',v',tx,p))
      end
        | cexp (A.EAPP (a,b,t,p)) =
      let
         val (ta,a') = cexp a
         val (tb,b') = cexp b

         val ta' = inst ta
         val tx = T.fresh ()

         val _ = TyConstraints.add (ta',T.FN([tb],tx),p)
      in
         (tx,A.EAPP (a',b',tx,p))
      end
        | cexp (A.ERAISE (e,p)) =
      let
         val (te,e') = cexp e
         val _ = TyConstraints.add(te,T.t_exn,p)
      in
         (T.fresh (),A.ERAISE (e',p))
      end
        | cexp (A.EHANDLE (pat,e,t,p)) =
      let
         val (tp,p') = cpat pat
         val (te,e') = cexp e
         val _ = TyConstraints.add (tp,T.t_exn,p)
      in
         (te,A.EHANDLE (p',e',te,p))
      end
        | cexp (A.ESEQ (v,t,p)) =
      let
         val (t',v') =
            Vector.foldl (fn (e,(lt,lv)) =>
               let
                  val (t,v) = cexp e
               in
                  (t,lv @ [v])
               end) (T.UNKNOWN,[]) v

         val _ = addOptConstraint (t',t,p)
      in
         (t',A.ESEQ (Vector.fromList v',t',p))
      end
        | cexp (A.EANN (e,t,p)) =
      let
         val (te,e') = cexp e
         val _ = addOptConstraint (te,t,p)
      in
         (te,e')
      end
        | cexp (A.ECON (i,NONE,t,p)) = (t,A.ECON (i,NONE,t,p))
        | cexp (A.ECON (i,SOME (e,t1),t2,p)) =
      let
         val (t',e') = cexp e
         val _ = TyConstraints.add (t',t1,p)
      in
         (t2,A.ECON (i,SOME (e',t'),t2,p))
      end
        | cexp (A.ESTUB (t,p)) = (t, A.ESTUB (t,p))
        | cexp (A.ELET (v,e,t,p)) = 
      let
         val v' = Vector.map cstm v
         val (t',e') = cexp e
      in
         (t',A.ELET (v',e',t',p))
      end
        | cexp (A.ECLOSURE (f,l,t,p)) =
      let
         val l' =
            map (fn (s,t) =>
                   case getType s of
                     NONE => Error.fail 
                              ("Invalid closure member '"^S.name s^"'",p)
                   | SOME t => (s,t)) l

         val (it,ot) = 
            case getType f of
               NONE => Error.fail
                  ("Invalid function in closure: '" ^ S.name f ^ "'",p)
             | SOME (T.FN x) => x
             | SOME _ =>
               Error.bug ("Constrain","Invalid closure non-function?",p)

         val nit = (map (fn (a,b) => b) l') @ it

         val nt = 
            List.foldr 
               (fn (it,ot) =>
                  T.FN ([it],ot)) ot nit

         val nt' = 
            case nt of T.FN (a,b) => b
                     | _ => 
                        Error.bug ("Constrain","Invalid n'",p)

         val t' = nt
      in
         (t',A.ECLOSURE (f,l',t',p))
      end
        | cexp e =
      Error.bug ("Constrain","Encountered unexpected expression: " ^
                             A.ppe e,(0,""))

      and cstm (A.DVAL (s,e,t,p)) =
      let
         (** Because we have made all symbols unique, there is no
             risk of accidental variable capture associated with
             recursive uses of definitions.  Therefore we can
             safely just bind the variable first *)
         val tx = T.fresh ()
         val _ = setType s tx
         val (te,e') = cexp e
         val _ = TyConstraints.add (tx,te,p)
         val _ = addOptConstraint (tx,t,p)
      in
         A.DVAL (s,e',tx,p)
      end
        | cstm (A.DVALREC (a,t,p)) = A.DVALREC (cstm a,t,p)
        | cstm (A.DMUTUAL (v,p)) = A.DMUTUAL (Vector.map cstm v, p)
        | cstm (A.DIMPORT (a,t,b,p)) =
      let
         val _ = setType b t
      in
         A.DIMPORT (a,t,b,p)
      end
        | cstm x = x
   in
      cstm de
   end
end
