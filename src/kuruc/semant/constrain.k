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

   fun vtol f = Vector.foldr (fn (a,b) => a::b) [] f

   fun prettyPrint c =
      String.concatWith "\n" (map (fn (a,b) => T.prettyPrint a ^ "\t\t:\t" ^
                                               T.prettyPrint b) c)

   (*fun inst tyS (T.POLY i) =
      (constraints := (map (fn (l,r) => 
         (T.substinty (T.POLY i) tyS l,
          T.substinty (T.POLY i) tyS r)) (!constraints));
          tyS)
     | inst tyS (T.FN (a,b)) = T.FN (inst tyS a, inst tyS b)
     | inst tyS (T.TUPLE l) = T.TUPLE (map (inst tyS) l)
     | inst tyS (T.CONSTR (l,t)) = T.CONSTR (map (inst tyS) l, inst tyS t)
     | inst tyS t = t*)

   fun tval_ty (A.LINT _) = T.t_int
     | tval_ty (A.LSTRING _) = T.t_string
     | tval_ty (A.LREAL _) = T.t_real
     | tval_ty (A.LCHAR _) = T.t_char
     | tval_ty (A.LUNIT _) = T.t_unit

   fun constr_pat te ve se (A.PCONST (e,_,p)) =
      let
         val (te',ve',se',cs,t,e') = constr_exp te ve se e
      in
         (te,ve,se,cs,t,A.PCONST(e',t,p))
      end
     | constr_pat te ve se (A.PID (s,t,p)) =
      let
         val t = T.fresh ()
         val ve' = S.enter (ve,s,E.VarEntry t)
      in
         (te,ve',se,[],t,A.PID(s,t,p))
      end
     | constr_pat te ve se (A.PTUPLE (v,t,p)) =
       let
          val (te',ve',se',cs',t',v') = List.foldl 
             (fn (e,(te,ve,se,cs,ts,es)) => 
               let
                  val (te',ve',se',cs',t',e') = constr_pat te ve se e
               in
                  (te',ve',se',cs@cs',ts@[t'],es@[e'])
               end) (te,ve,se,[],[],[]) (vtol v)

          val v'' = Vector.fromList v'
          val t'' = T.TUPLE t'
       in
          (te',ve',se',cs',t'',A.PTUPLE(v'',t'',p))
       end

     | constr_pat _ _ _ _ = raise Fail "constr_pat not implemented"

   and constr_exp te ve se (A.ELIT(tv,_,p)) = 
       let
          val t = tval_ty tv
       in
          (te,ve,se,[],t,A.ELIT(tv,t,p))
       end
     | constr_exp te ve se (A.EID (s,t,p)) =
       let
          val t' = case (S.look (ve,s)) of NONE => raise IdUnknown (S.name s,p) 
                                         | SOME (E.VarEntry k) => k
       in
          (te,ve,se,[],t',A.EID (s,t',p))
       end
     | constr_exp te ve se (A.ETUPLE (v,t,p)) =
       let
          val v' = map (constr_exp te ve se) (vtol v)

          val v'' = Vector.fromList (map (fn (_,_,_,_,_,e) => e) v')
          val t = T.TUPLE (map (fn (_,_,_,_,t,_) => t) v')
			 val cs' = List.concat (map (fn (_,_,_,c,_,_) => c) v')
       in
          (te,ve,se,cs',t,A.ETUPLE(v'',t,p))
       end
     | constr_exp te ve se (A.EFN (s,e,t,p)) =
       let
          (* We've transformed all functions to factor out patterns.
             As a result, any type information attached
             to the EFN will be an FN type ta -> tb, which we add
             as a constraint on our inferred type, t' -> tx *)
          
          val t' = T.fresh ()
          val ve' = S.enter (ve,s,E.VarEntry t')
          
          val (te',ve'',se',cs,t2,e') = constr_exp te ve' se e

          val tx = T.FN (t',t2) 
       in
          (te,ve,se,cs,tx,A.EFN (s,e',tx,p))
       end
     | constr_exp te ve se (A.EAPP (e1,e2,t,p)) =
       let
          val (te',ve',se',cs1,te2,e2') = constr_exp te ve se e2
          val (te',ve',se',cs2,te1,e1') = constr_exp te ve se e1
          
          (*val te1' = inst (T.fresh ()) te1 *)
			 val te1' = te1

          val tx = T.fresh ()
          
          val cs' = [(te1',T.FN(te2,tx))]
       in
          (te,ve,se, cs' @ cs1 @ cs2, tx, A.EAPP(e1',e2',tx,p))
       end
     | constr_exp te ve se (A.ECASE (e1,v,t,p)) =
       let
          val (te',ve',se',cs, t1,e1') = constr_exp te ve se e1

          val rt = T.fresh ()

			 val kcs = ref cs

          val v' = Vector.fromList (
                      map (fn (p,e) =>
                        let
                           val (te',ve',se',pcs',pt,p') = constr_pat te ve se p
                           val (_,_,_,ecs',et,e') = constr_exp te' ve' se' e
                 				val _ = kcs := (!kcs) @ pcs' @ ecs'
					  			in
                           (p',e')
                        end) (vtol v)
                   )
       in
          (te,ve,se,!kcs,rt,A.ECASE (e1',v',rt,p))
       end
     | constr_exp te ve se (A.ESEQ (v,t,p)) =
       let
          val rt = T.fresh ()

          val v' = Vector.map (fn el => 
                           let
                              val (te',ve',se',cs',t',el') = 
												constr_exp te ve se el
                           in
                              (t',el')
                           end) v

          val (lt,le) = Vector.sub (v', Vector.length v' - 1)
          val cs = [(rt,lt)]
          val v'' = Vector.map (fn (a,b) => b) v'
       in
          (te,ve,se,cs,rt,A.ESEQ (v'',rt,p))
       end
     | constr_exp te ve se (A.ELET (v,e,t,p)) =
       let
          val (te',ve',se',cs,v') = Vector.foldl (fn (d,(te',ve',se',cs',d')) => 
                                 let
                                    val (te'',ve'',se'',cs'',_,d'') = 
                                          constr_dec te' ve' se' false d
                                 in
                                    (te'',ve'',se'',cs'@cs'',d'@[d''])
                                 end) (te,ve,se,[],[]) v

          val (_,_,_,cs',t',e') = constr_exp te' ve' se' e
       in  
          (te,ve,se,cs@cs',t',A.ELET(Vector.fromList v',e',t',p))
       end
     | constr_exp _ _ _ e = raise Fail ("Unhandled constr_exp:" ^ A.ppe e)

   (* The flag in the 4th argument indicates whether to bind the name
      first or not - true = val rec, false = val *)
   and constr_dec te ve se false (A.DVAL (s,e,t,p)) =
       let
          val (te',ve',se',cs',t',e') = constr_exp te ve se e
          val ve'' = S.enter (ve,s,E.VarEntry t')
       in
          (te,ve'',se,cs',t',A.DVAL (s,e',t',p))
       end
     | constr_dec te ve se true (A.DVAL (s,e,t,p)) =
       let
          val tx = T.fresh () 
          val ve'' = S.enter (ve,s,E.VarEntry tx)
          
          val (te',ve',se',cs',t',e') = constr_exp te ve'' se e

          val cs = [(tx,t')]
       in
          (te,ve',se,cs'@cs,t',A.DVAL (s,e',t',p))
       end
     | constr_dec te ve se b (A.DVALREC (a,t,p)) =
         constr_dec te ve se true a
     | constr_dec _ _ _ _ _ = raise Fail "Unhandled constr_dec"

   fun substinenv (tn,tr) ve =
      let
			val vkeys = S.keys ve 
		
			fun upd ve NONE _ = 
				raise Fail "[BUG] constr updates unknown symbol"
			  | upd ve (SOME s) t = 
			  		S.enter (ve,s,t)
		in
			(List.foldl (fn ((s,Env.VarEntry t),ve') =>
				upd ve' (S.unhash s) 
					(Env.VarEntry (T.substinty tn tr t)))
				) ve vkeys
		end

	fun generateConstraints' te ve se c =
      List.foldl (fn (d,(te',ve',se',cs',d')) =>
                      let
                         val _ = Debug.print Debug.debug ("Constraining: " ^
                                    A.prettyPrint [d] ^ "\n")
                         val (te'',ve'',se'',cs'',_,d'') = 
								 			constr_dec te' ve' se' false d
                         val _ = Debug.print Debug.debug "Value Environment:\n"
                         val _ = Debug.print Debug.debug (S.prettyPrint 
                                    (fn (E.VarEntry t) => T.prettyPrint t) ve'')
                         val _ = Debug.print Debug.debug "\n"

                         val cs''' = Unify.unify (cs'')

						      val _ = Debug.print Debug.internal 
                                 "Constraint Set:\n================\n"
                         val _ = Debug.print Debug.internal 
                                 ("\n" ^prettyPrint (cs'') ^"\n")
                         val _ = Debug.print Debug.internal 
                                 "\n================\n"


                         val _ = Debug.print Debug.internal 
                                 "Constraint Set:\n================\n"
                         val _ = Debug.print Debug.internal 
                                 ("\n" ^prettyPrint (cs''') ^"\n")
                         val _ = Debug.print Debug.internal 
                                 "\n================\n"

                         (*val ve''' = List.foldl (fn (cstr,ve) => 
                                         substinenv cstr ve) ve'' (!constraints)
								*)
                      in
                         (te'',ve'',se'',cs'''@cs',
								 	Unify.apply (cs''') (d'@[d'']))
                      end)
                        (te,ve,se,[],[]) c

   fun generateConstraints tenv venv senv c =
      let
         val (te,ve,se,cn,k) = generateConstraints' tenv venv senv c

         val _ = print (A.prettyPrint k)
      in
         (te,ve,se,cn,k)
      end handle IdUnknown (s,p) => raise Fail ("Unknown identifier: " ^
                                                s ^ " near " ^
                                                Int.toString p)
end



