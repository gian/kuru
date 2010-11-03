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
*  Do the final conversion from ParseTree to TypedAST.
******************************************************************************)

structure AstParseTree =
struct
   open ParseTree
   structure A = TypedAST
   structure S = Symbol
   structure T = Type

   exception IllegalParseTree of string * pos

   (* TODO: need to preserve the path in some other TypedAST element... *)
   fun resolveLongId te ve se (LongId (Ident (s,_),r,p)) = 
         (case (S.look (se,S.symbol s)) of
               NONE => raise Fail ("Unknown structure id: " ^ s)
             | SOME (Env.StructEntry (te',ve',se')) => 
                     (Symbol.symbol s) :: 
                        resolveLongId te' ve' se' r)
     | resolveLongId te ve se (Ident (s,_)) = [Symbol.symbol s] 
     | resolveLongId te ve se _ = raise Fail "Not implemented resolveLongId"

   fun convert tenv venv senv pt = 
   let
      fun t te ve se (Var (Ident (a,_),p)) = T.VAR (S.symbol a)
        | t te ve se (TyArrow (a,b,p)) = T.FN (t te ve se a, t te ve se b)
        | t te ve se (TyPair (a,b,p)) = raise Fail "TyPair not implemented"
        | t te ve se (TyCon (a,b,p)) = T.CONSTR ([t te ve se a], t te ve se b)
        | t te ve se (TyName (Ident (a,_),p)) = T.NAME (S.symbol a) 
        | t te ve se n = raise Fail ("Unknown type not handled: " ^ 
                                    ParseTree.prettyPrint n)

      and cpat te ve se (AsPat (a,b,p)) = raise Fail "aspat not implemented"
        | cpat te ve se (InfixPat (a,Ident (n,p),c,pos)) =  
        		 (te,ve,A.PINFIX ((fn (_,_,e) => e) (cpat te ve se a), 
				 						S.symbol n, 
										(fn (_,_,e) => e) (cpat te ve se c), 
										T.UNKNOWN, 
										p))
		  | cpat te ve se (TuplePat (l,p)) =
             (te,ve,A.PTUPLE (Vector.fromList (
                                List.map 
                                 (fn le => 
                                    (fn (_,_,e) => e) (cpat te ve se le)) l),
                             T.UNKNOWN, p))
        | cpat te ve se (ListPat (l,p)) =
             (te,ve,A.PLIST (Vector.fromList (
                                List.map 
                                 (fn le => 
                                    (fn (_,_,e) => e) (cpat te ve se le)) l),
                             T.UNKNOWN, p))
        | cpat te ve se (ConPat (a,p)) =
            (te,ve,A.PCONST (s te ve se a, T.UNKNOWN, p))
        | cpat te ve se (ConstrPat (Ident (n,_),NONE,p)) =
          let
             val s' = S.symbol n
             val t = case (S.look (ve,s')) of
                        SOME (Env.VarEntry t') => t'
                      | NONE => raise IllegalParseTree ("Constr " ^ n, p)
          in
             (te,ve,A.PCONSTR (S.symbol n,NONE,t,p))
          end
        | cpat te ve se (ConstrPat (a,SOME b,p)) = 
          let
             val s' = S.symbol "_DUMMY_CONSTR_" 
             val t = case (S.look (ve,s')) of
                        SOME (Env.VarEntry t') => t'
                      | NONE => raise IllegalParseTree ("Constr n" , p)
          in
             (te,ve,A.PCONSTR (S.symbol "_DUMMY_CONSTR",NONE,t,p))
          end
        | cpat te ve se (TyAnnPat (a,b,p)) = raise Fail "TyAnnPat not impl"
        | cpat te ve se (IdPat (Ident (n,_),p)) =
          let
             val s' = S.symbol n
             val ve' = S.enter (ve,s',Env.VarEntry T.UNKNOWN) 
          in
             (te,ve',A.PID (s',T.UNKNOWN,p))
          end
        | cpat te ve se (IdPat (_,p)) = raise IllegalParseTree ("IdPat",p)
        | cpat te ve se (WildcardPat p) = 
            (te,ve,A.PWILD p)
        | cpat te ve se (NilPat p) = (te,ve,A.PNIL p)
        | cpat te ve se p = 
            raise IllegalParseTree ("cpat Match: " ^ prettyPrint p,0)

      and s te ve se ((TupleExp (l,p)) : pt) : A.texp = 
            A.ETUPLE (Vector.fromList (map (s te ve se) l),T.UNKNOWN,p) : A.texp
        | s te ve se (AndAlso (a,b,p)) = raise IllegalParseTree ("andalso",p)
        | s te ve se (OrElse (a,b,p)) = raise IllegalParseTree ("orelse",p)
        | s te ve se (Infix (a,opr,b,p)) = raise IllegalParseTree ("orelse",p)
        | s te ve se (LongId (a,b,p)) = 
            A.ELID (resolveLongId te ve se (LongId (a,b,p)),T.UNKNOWN,p)
        | s te ve se (Var (a,p)) = raise IllegalParseTree ("var",p) 
        | s te ve se (Raise (a : pt,p)) = A.ERAISE (s te ve se a,p) : A.texp
        | s te ve se (Seq (a,b,p)) = raise IllegalParseTree ("Seq",p) 
        | s te ve se (OpExp (a,p)) = raise IllegalParseTree ("OpExp",p) 
        | s te ve se (App (a,b,p)) = A.EAPP (s te ve se a, s te ve se b,T.UNKNOWN,p)
        | s te ve se (TyAnn (a,b,p)) = A.EANN (s te ve se a, t te ve se b, p)
        | s te ve se (SeqExp (l,p)) =
            A.ESEQ (Vector.fromList (map (s te ve se) l),T.UNKNOWN,p) : A.texp
        | s te ve se (ListExp (l,p)) =
            raise Fail ("ast_parse_tree: ListExp node remains!")
        | s te ve se (NilExp p) = A.ENIL p
        | s te ve se (LetExp (l,e,p)) = 
          let
             val (te',ve',se',dl) = convert te ve se l
          in
             A.ELET (Vector.fromList dl,s te' ve' se e,T.UNKNOWN,p)
          end
        | s te ve se (IfExp (a,b,c,p)) = 
             A.EIF (s te ve se a, s te ve se b, s te ve se c, T.UNKNOWN, p)
        | s te ve se (WhileExp (a,b,p)) = 
            A.EWHILE (s te ve se a, s te ve se b,T.UNKNOWN,p)
        | s te ve se (CaseExp (a,m,p)) = 
          let
             val m' = Vector.fromList(map (fn (Match (a,b,_)) =>
                               let
                                  val (te',ve',a') = cpat te ve se a
                               in (a', s te' ve' se b) end
                                             | _ => 
                                               raise Fail 
                                                "non-Match in case expression") m)
          in
             A.ECASE (s te ve se a, m', T.UNKNOWN, p)
          end
        | s te ve se (FnExp ([Match (IdPat (Ident (n,_),_),e,_)],p)) = 
          let
             val ve' = Symbol.enter (ve,S.symbol n,Env.VarEntry (T.UNKNOWN))
          in
            A.EFN (S.symbol n, s te ve' se e, T.UNKNOWN, p)
          end
        | s te ve se (FnExp (_,p)) = raise IllegalParseTree ("FnExp",p)
        | s te ve se (HandleExp (e,l,p)) = raise Fail "handle not implemented" 
        | s te ve se (Match (a,b,p)) = raise IllegalParseTree ("Match",p)
        | s te ve se (Int (b,n,p)) = 
            A.ELIT(A.LINT ((if b then ~1 * (valOf (Int.fromString n))
                                else valOf (Int.fromString n)), p),T.t_int, p)
        | s te ve se (Real _) = raise Fail "Real not impl."
        | s te ve se (Char (n,p)) = 
            A.ELIT(A.LCHAR (String.sub (n,0),p),T.t_char,p)
        | s te ve se (String (n,p)) =
            A.ELIT(A.LSTRING (n,p),T.t_string,p)
        | s te ve se (UnitExp p) = A.ELIT(A.LUNIT p, T.t_unit, p)
        | s te ve se (Ident (x,p)) = A.EID (Symbol.symbol x,T.UNKNOWN,p)
        | s te ve se pt = raise Fail ("expression match failed in: " ^ pp pt)

      fun d te ve se (ValDec (l,p)) =
          let
             fun prc [] = raise IllegalParseTree ("Empty ValDec", p)
               | prc [b] = d te ve se b
               | prc l =
                 let
                    val (te',ve',r) =  
                     List.foldl (fn (bind, (te',ve',r)) => 
                                 let
                                    val (te'',ve'',se'',r') = d te' ve' se bind
                                 in (te'',ve'',r@r') end)
                                    (te,ve,[]) l
             
                 in (te',ve',se,[A.DMUTUAL(Vector.fromList r, p)]) end
          in
             prc l
          end
        | d te ve se (Seq (a,b,p)) =
          let
             fun f (Seq (a,b,p)) = f a @ f b
               | f a = [a]

             val l = f a @ f b

             val (te',ve',se',l') = 
               List.foldl (fn (e,(te,ve,se,m)) => 
                              let
                                 val (te',ve',se',m') = d te ve se e
                              in
                                 (te',ve',se',m @ m')
                              end) (te,ve,se,[]) l

             val _ = Debug.print Debug.debugmore ("ast-parse-tree: Seq: " ^
                                                   Int.toString (length l) ^ "\n")

          in
             (te',ve',se',l')
          end
        | d te ve se (ValRec ([],p)) = raise IllegalParseTree ("ValRec[]",p)
        | d te ve se (ValRec ([b],p)) = 
          let
             val (te',ve',se',b') = d te ve se b
          in (te',ve',se,[A.DVALREC (hd b',T.UNKNOWN,p)]) end
        | d te ve se (ValRec (l,p)) =
          let
             val (te',ve',r) =  
                     List.foldl (fn (bind, (te',ve',r)) => 
                                 let
                                    val (te'',ve'',se'',r') = d te' ve' se bind
                                 in (te'',ve'',r@r') end)
                                    (te,ve,[]) l
             
          in 
             (te',ve',se,[A.DVALREC(A.DMUTUAL(Vector.fromList r, p),T.UNKNOWN,p)]) 
          end
        | d te ve se (TypeDec (l,p)) =
          let
             val (te',ve',_,_) = 
                      List.foldl (fn (b,(te',ve',se',_)) => d te' ve' se b) 
                        (te,ve,se,[]) l
          in
             (te',ve',se,[])
          end
        | d te ve se (DatatypeAssign (a,b,p)) = 
               raise Fail "DatatypeAssign not implemented"
        | d te ve se (DatatypeDec (l,p)) =
          let
             val (te',ve',r) =  
                     List.foldl (fn (bind, (te',ve',r)) => 
                                 let
                                    val (te'',ve'',se'',r') = d te' ve' se bind
                                 in (te'',ve'',r@r') end)
                                    (te,ve,[]) l
             
          in (te',ve',se,[A.DMUTUAL(Vector.fromList r,p)]) end
        | d te ve se (FunDec (ll,p)) = raise IllegalParseTree ("FunDec",p)
        | d te ve se (ValBind (a,b,p)) =
          let
             val (te',ve',(s',_,_)) = 
               case (cpat te ve se a) of (t,v,A.PID x) => (t,v,x)
                                    | _ => raise IllegalParseTree ("ValBind",p)
             val e = s te' ve' se b
          in
             (te',ve',se,[A.DVAL (s',e,T.UNKNOWN,p)])
          end
        | d te ve se (TypeBind ([],a,b,p)) =  
          let
             val a' = case (t te ve se a) of T.NAME n => n
                                        | _ => raise IllegalParseTree ("TypeBind",p)
             val b' = t te ve se b
             val te' = Symbol.enter (te,a',b')
          in
             (te',ve,se,[])
          end

        | d te ve se (TypeBind (l,a,b,p)) = 
          let
             val l' = map (t te ve se) l
             val a' = case (t te ve se a) of T.NAME n => n
                                        | _ => raise IllegalParseTree ("TypeBind",p)
             val b' = T.CONSTR(l',t te ve se b)
             val te' = Symbol.enter (te,a',b')
          in
             (te',ve,se,[])
          end
        | d te ve se (InfixDec _) = (te, ve, se, [])
        | d te ve se (InfixrDec _) = (te, ve, se, [])
        | d te ve se (FunMatch (a,l,_,c,p)) =
             raise IllegalParseTree ("FunMatch",p)
        | d te ve se (DataBind (l,a,b,p)) =
          let
             val a' = case (s te ve se a) of A.EID (n,t,p) => n
                                        | _ => raise Fail "non-id DataBind"
             
             val te' = S.enter (te,a',T.UNION (a',[]))

             val cbi = ref 0

             val (te'',ve',ins) = 
                List.foldl (fn (cb, (te,ve,ins)) =>
                   let
                      val (te',ve',ins') = 
                        (case cb of
                           ConBind (Ident (id,pos),NONE,_) =>
                              let
                                 val ve' = S.enter (ve,S.symbol id,
                                                      Env.VarEntry (T.NAME a'))
                                 val te' = (case S.look (te,a') of
                                               SOME (T.UNION (n',l)) =>
                                                  S.enter (te,
                                                            a',
                                                            T.UNION (n',
                                                            l @ [T.NAME 
                                                                 (S.symbol id)]))
                                             | _ => raise Fail 
                                                   "ast_parse_tree: UNION bug")
                                 val ins' =
                                             [A.DVAL (S.symbol id,
                                                      A.ECON (!cbi,NONE,
                                                              T.NAME a',
                                                              pos),
                                                      T.NAME a',pos)]
                                 val _ = cbi := !cbi + 1
                              in
                                 (te',ve',ins')
                              end
                         | ConBind (Ident (id,pos),SOME f,_) =>
                           let
                              val f' = t te ve se f

                              val ve' = S.enter (ve,S.symbol id,
                                           Env.VarEntry (T.FN (f',
                                                               T.NAME a')))

                              val te' = (case S.look (te,a') of
                                               SOME (T.UNION (n',l)) =>
                                                  S.enter (te,
                                                            a',
                                                            T.UNION (n',
                                                            l @ 
                                                            [T.FN (f',
                                                               T.NAME
                                                               (S.symbol id))]))
                                             | _ => raise Fail 
                                                   "ast_parse_tree: UNION bug")

                              val p = S.symbol ("_p" ^ Int.toString (!cbi)) 
                              val ins' = [A.DVAL (S.symbol id,
                                             A.EFN (p,
                                               A.ECON (!cbi,
                                                SOME (A.EID (p,f',pos)),
                                                T.NAME a',
                                                pos),
                                             T.FN (f',T.NAME a'),pos),
                                             T.FN (f',T.NAME a'),pos)]
                              val _ = cbi := !cbi + 1 
                           in
                               (te',ve',ins')
                           end
                         | _ => raise Fail "ast-parse-tree: Non-ConBind?")
                   in
                      (te',ve',ins @ ins')
                   end) (te',ve,[]) b
          in
             (te'',ve',se,ins)
          end
        | d te ve se (ConBind (a,_,p)) = raise Fail "ConBind not implemented"
        | d te ve se (StructDec ([b],p)) =  
          let
             val (te',ve',se',b') = d te ve se b
          in
             (te,ve,se',b')
          end
        | d te ve se (StructDec (l,p)) =
          let
             val (te',ve',se',l') = convert te ve se l
          in
             (te,ve,se',[A.DMUTUAL (Vector.fromList l',p)])
          end
        | d te ve se (StructBind (a,Structure (l,p1),p2)) =
          let
             val a' = case (s te ve se a) of A.EID (n,t,p) => n
                                        | _ => raise Fail "non-id StructBind"
             val (te',ve',se',l') = convert te ve se l

             val se'' = S.enter (se,a',Env.StructEntry (te',ve',se'))
          in
             (te,ve,se'',[A.DSTRUCTURE (a',Vector.fromList l',p1)])
          end
        | d te ve se (StructBind (a,b,p)) =
          let
             fun sb se [] = raise Fail ("Empty long-id in struct assign near: " ^
                                       Int.toString p)
               | sb se [h] = (case (S.look (se,h)) of 
                                 (SOME e) => e
                               | NONE => raise Fail ("Invalid id in struct " ^
                                                     "assign near: " ^
                                                     Int.toString p))
               | sb se (h::t) = (case (S.look (se,h)) of
                                  (SOME (Env.StructEntry (_,_,e))) => sb e t
                                | NONE => raise Fail ("Invalid id in struct " ^
                                                      "assign near: " ^ 
                                                      Int.toString p))



             val a' = case (s te ve se a) of A.EID (n,t,p') => n
                                           | A.ELID (nl,t,p') =>
                                             raise Fail("Unexpected long-id: " ^
                                                      Int.toString p)

             val b' = s te ve se b
             val b'' = case b' of A.EID (n,t,p') => sb se [n]
                                | A.ELID (nl,t,p') => sb se nl

             val se' = S.enter (se,a',b'')
          in
             (te,ve,se',[A.DSTRUCTASSIGN (a',b',p)])
          end
        | d te ve se t = raise Fail "decl match"
   in
        List.foldl (fn (a,(te',ve',se',k)) =>
                     let
                        val (te'',ve'',se'',k') = d te' ve' se' a 
                     in
                        (te'',ve'',se'',k@k')
                     end) (tenv,venv,senv,[]) pt
   end handle IllegalParseTree (m,p) => raise Fail ("Illegal Parse Tree: " ^ m ^
																	 " near " ^ Int.toString p)
end

