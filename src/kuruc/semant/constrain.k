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
*    Generate type constraints from pre-scrubbed ASTs. 
******************************************************************************)
structure Constrain =
struct
	structure S = Symbol
	structure T = Type
	structure E = Env
	structure A = TypedAST

   type pos = A.pos

   (* Exceptions *)
   exception IdUnknown of string * pos

   val constraints = ref [] : (T.ty * T.ty) list ref 

   (* TODO: Add position to constraint so we can report a position when
      type unification fails *)
   fun add_constr (k,v) = constraints := (!constraints) @ [(k,v)]
   fun vtol f = Vector.foldr (fn (a,b) => a::b) [] f

   fun prettyPrint c =
      String.concatWith "\n" (map (fn (a,b) => T.prettyPrint a ^ "\t\t:\t" ^
                                               T.prettyPrint b) c)

   fun tval_ty (A.LINT _) = T.t_int
     | tval_ty (A.LSTRING _) = T.t_string
     | tval_ty (A.LREAL _) = T.t_real
     | tval_ty (A.LCHAR _) = T.t_char
     | tval_ty (A.LUNIT _) = T.t_unit

   fun constr_pat te ve se (A.PCONST (e,_,p)) =
      let
         val (te',ve',se',t,e') = constr_exp te ve se e
      in
         (te,ve,se,t,A.PCONST(e',t,p))
      end
     | constr_pat te ve se (A.PID (s,t,p)) =
      let
         val t = T.fresh ()
         val ve' = S.enter (ve,s,E.VarEntry t)
         val _ = case t of T.UNKNOWN => ()
                         | t' => add_constr (t,t')
      in
         (te,ve',se,t,A.PID(s,t,p))
      end
     | constr_pat te ve se (A.PTUPLE (v,t,p)) =
       let
          val (te',ve',se',t',v') = List.foldl 
             (fn (e,(te,ve,se,ts,es)) => 
               let
                  val (te',ve',se',t',e') = constr_pat te ve se e
               in
                  (te',ve',se',ts@[t'],es@[e'])
               end) (te,ve,se,[],[]) (vtol v)

          val v'' = Vector.fromList v'
          val t'' = T.TUPLE t'
       in
          (te',ve',se',t'',A.PTUPLE(v'',t'',p))
       end

     | constr_pat _ _ _ _ = raise Fail "constr_pat not implemented"

   and constr_exp te ve se (A.ELIT(tv,_,p)) = 
       let
          val t = tval_ty tv
       in
          (te,ve,se,t,A.ELIT(tv,t,p))
       end
     | constr_exp te ve se (A.EID (s,t,p)) =
       let
          val t' = case (S.look (ve,s)) of NONE => raise IdUnknown (S.name s,p) 
                                         | SOME (E.VarEntry k) => k
          val _ = case t of T.UNKNOWN => ()
                          | t' => add_constr (t,t')
       in
          (te,ve,se,t',A.EID (s,t',p))
       end
     | constr_exp te ve se (A.ETUPLE (v,t,p)) =
       let
          val v' = map (constr_exp te ve se) (vtol v)

          val v'' = Vector.fromList (map (fn (_,_,_,_,e) => e) v')
          val t = T.TUPLE (map (fn (_,_,_,t,_) => t) v')
       in
          (te,ve,se,t,A.ETUPLE(v'',t,p))
       end
     | constr_exp te ve se (A.EFN (s,e,t,p)) =
       let
          (* We've transformed all functions to factor out patterns.
             As a result, any type information attached
             to the EFN will be an FN type ta -> tb, which we add
             as a constraint on our inferred type, t' -> tx *)
          
          val t' = T.fresh ()
          val ve' = S.enter (ve,s,E.VarEntry t')
          val tx = T.fresh () (* Result type *)

          val (te',ve'',se',t2,e') = constr_exp te ve' se e

          val _ = add_constr (tx,t2)

          val ft = T.FN(t',tx)

          val _ = case t of T.UNKNOWN => ()
                          | t'' => add_constr (ft,t'')
       in
          (te,ve,se,ft,A.EFN (s,e',ft,p))
       end
     | constr_exp te ve se (A.EAPP (e1,e2,t,p)) =
       let
          (* We get a type for e2 and then enforce a type for e1 that is:
             t(e2) -> tx, where tx = t(e1 e2) *)
          val tx = T.fresh ()
          val (te',ve',se',te1,e1') = constr_exp te ve se e1
          val (te',ve',se',te2,e2') = constr_exp te ve se e2
          val _ = add_constr(te1,T.FN(te2,tx))
       in
          (te,ve,se,tx,A.EAPP(e1',e2',tx,p))
       end
     | constr_exp te ve se (A.ECASE (e1,v,t,p)) =
       let
          val (te',ve',se',t1,e1') = constr_exp te ve se e1

          val rt = T.fresh ()

          val v' = Vector.fromList (
                      map (fn (p,e) =>
                        let
                           val (te',ve',se',pt,p') = constr_pat te ve se p
                           val (_,_,_,et,e') = constr_exp te' ve' se' e
                           val _ = add_constr (t1,pt)
                           val _ = add_constr (rt,et)
                        in
                           (p',e')
                        end) (vtol v)
                   )
       in
          (te,ve,se,rt,A.ECASE (e1',v',rt,p))
       end
     | constr_exp te ve se (A.ESEQ (v,t,p)) =
       let
          val rt = T.fresh ()

          val v' = Vector.map (fn el => 
                           let
                              val (te',ve',se',t',el') = constr_exp te ve se el
                           in
                              (t',el')
                           end) v

          val (lt,le) = Vector.sub (v', Vector.length v' - 1)
          val _ = add_constr (rt,lt)
          val v'' = Vector.map (fn (a,b) => b) v'
       in
          (te,ve,se,rt,A.ESEQ (v'',rt,p))
       end
     | constr_exp te ve se (A.ELET (v,e,t,p)) =
       let
          val (te',ve',se',v') = Vector.foldl (fn (d,(te',ve',se',d')) => 
                                 let
                                    val (te'',ve'',se'',_,d'') = 
                                          constr_dec te' ve' se' false d
                                 in
                                    (te'',ve'',se'',d'@[d''])
                                 end) (te,ve,se,[]) v

          val (_,_,_,t',e') = constr_exp te' ve' se' e

          val _ = case t of T.UNKNOWN => ()
                          | t'' => add_constr (t',t'')
       in  
          (te,ve,se,t',A.ELET(Vector.fromList v',e',t',p))
       end
     | constr_exp _ _ _ e = raise Fail ("Unhandled constr_exp:" ^ A.ppe e)

   (* The flag in the 4th argument indicates whether to bind the name
      first or not - true = val rec, false = val *)
   and constr_dec te ve se false (A.DVAL (s,e,t,p)) =
       let
          val (te',ve',se',t',e') = constr_exp te ve se e
          val tx = T.fresh ()
          val _ = add_constr (tx,t')
          val _ = case t of T.UNKNOWN => ()
                          | t'' => add_constr (tx,t'')
          val ve'' = S.enter (ve,s,E.VarEntry tx)
       in
          (te,ve'',se,tx,A.DVAL (s,e',tx,p))
       end
     | constr_dec te ve se true (A.DVAL (s,e,t,p)) =
       let
          val tx = T.fresh ()
          val ve'' = S.enter (ve,s,E.VarEntry tx)
          
          val (te',ve',se',t',e') = constr_exp te ve'' se e
         
         val _ = add_constr (tx,t')
         
         val _ = case t of T.UNKNOWN => ()
                          | t'' => add_constr (tx,t'')

       in
          (te,ve'',se,tx,A.DVAL (s,e',tx,p))
       end
     | constr_dec te ve se b (A.DVALREC (a,t,p)) =
         constr_dec te ve se true a
     | constr_dec _ _ _ _ _ = raise Fail "Unhandled constr_dec"

	fun generateConstraints' te ve se c =
      List.foldl (fn (d,(te',ve',se',d')) =>
                      let
                         val _ = Debug.print Debug.debug ("Constraining: " ^
                                    A.prettyPrint [d] ^ "\n")
                         val (te'',ve'',se'',_,d'') = constr_dec te' ve' se' false d
                         val _ = Debug.print Debug.debug "Value Environment:\n"
                         val _ = Debug.print Debug.debug (S.prettyPrint 
                                    (fn (E.VarEntry t) => T.prettyPrint t) ve'')
                         val _ = Debug.print Debug.debug "\n"
                      in
                         (te'',ve'',se'',d'@[d''])
                      end)
                        (te,ve,se,[]) c

   fun generateConstraints tenv venv senv c =
      let
         val (te,ve,se,k) = generateConstraints' tenv venv senv c
         val _ = Debug.print Debug.internal "Constraint Set:\n================\n"
         val _ = Debug.print Debug.internal ("\n" ^prettyPrint (!constraints) ^"\n")
         val _ = Debug.print Debug.internal "\n================\n"
         val cn = !constraints
         val _ = constraints := []
      in
         (te,ve,se,cn,k)
      end handle IdUnknown (s,p) => raise Fail ("Unknown identifier: " ^
                                                s ^ " near " ^
                                                Int.toString p)
end



