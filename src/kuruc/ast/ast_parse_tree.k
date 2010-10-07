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

   fun resolveLongId te ve (a,b) = raise Fail "longid not implemented"

   fun convert tenv venv senv pt = 
   let
      fun t te ve (Var (Ident (a,_),p)) = T.VAR (S.symbol a)
        | t te ve (TyArrow (a,b,p)) = T.FN (t te ve a, t te ve b)
        | t te ve (TyPair (a,b,p)) = raise Fail "TyPair not implemented"
        | t te ve (TyCon (a,b,p)) = T.CONSTR ([t te ve a], t te ve b)
        | t te ve (TyName (Ident (a,_),p)) = T.NAME (S.symbol a) 
        | t te ve _ = raise Fail "Unknown type not handled"

      and cpat te ve (AsPat (a,b,p)) = raise Fail "aspat not implemented"
        | cpat te ve (InfixPat (a,b,c,pos)) =  
            raise Fail "InfixPat not implemented"
        | cpat te ve (TuplePat (l,p)) =
             (te,ve,A.PTUPLE (Vector.fromList (
                                List.map (fn le => (fn (_,_,e) => e) (cpat te ve le)) l),
                             T.UNKNOWN, p))
        | cpat te ve (ListPat (l,p)) =
             (te,ve,A.PLIST (Vector.fromList (
                                List.map (fn le => (fn (_,_,e) => e) (cpat te ve le)) l),
                             T.UNKNOWN, p))
        | cpat te ve (ConPat (a,p)) =
            (te,ve,A.PCONST (s te ve a, T.UNKNOWN, p))
        | cpat te ve (ConstrPat (Ident (n,_),NONE,p)) =
          let
             val s' = S.symbol n
             val t = case (S.look (ve,s')) of
                        SOME (Env.VarEntry t') => t'
                      | NONE => raise IllegalParseTree ("Constr " ^ n, p)
          in
             (te,ve,A.PCONSTR (S.symbol n,NONE,t,p))
          end
        | cpat te ve (ConstrPat (a,SOME b,p)) = 
          let
             val s' = S.symbol "_DUMMY_CONSTR_" 
             val t = case (S.look (ve,s')) of
                        SOME (Env.VarEntry t') => t'
                      | NONE => raise IllegalParseTree ("Constr n" , p)
          in
             (te,ve,A.PCONSTR (S.symbol "_DUMMY_CONSTR",NONE,t,p))
          end

        | cpat te ve (TyAnnPat (a,b,p)) = raise Fail "TyAnnPat not impl"
        | cpat te ve (IdPat (Ident (n,_),p)) =
          let
             val s' = S.symbol n
             val t = T.fresh()
             val ve' = S.enter (ve,s',Env.VarEntry t)
          in
             (te,ve',A.PID (s',t,p))
          end
        | cpat te ve (IdPat (_,p)) = raise IllegalParseTree ("IdPat",p)
        | cpat te ve _ = raise IllegalParseTree ("cpat Match",0)

      and s (te : T.ty S.table) (ve : Env.enventry S.table) ((TupleExp (l,p)) : pt) : A.texp = 
            A.ETUPLE (Vector.fromList (map (s te ve) l),T.UNKNOWN,p) : A.texp
        | s te ve (AndAlso (a,b,p)) = raise IllegalParseTree ("andalso",p)
        | s te ve (OrElse (a,b,p)) = raise IllegalParseTree ("orelse",p)
        | s te ve (Infix (a,opr,b,p)) = raise IllegalParseTree ("orelse",p)
        | s te ve (LongId (a,b,p)) = raise Fail "LongId not implemented" 
        | s te ve (Var (a,p)) = raise IllegalParseTree ("var",p) 
        | s te ve (Raise (a : pt,p)) = A.ERAISE (s te ve a,p) : A.texp
        | s te ve (Seq (a,b,p)) = raise IllegalParseTree ("Seq",p) 
        | s te ve (OpExp (a,p)) = raise IllegalParseTree ("OpExp",p) 
        | s te ve (App (a,b,p)) = A.EAPP (s te ve a, s te ve b,T.UNKNOWN,p)
        | s te ve (TyAnn (a,b,p)) = A.EANN (s te ve a, t te ve b, p)
        | s te ve (SeqExp (l,p)) =
            A.ESEQ (Vector.fromList (map (s te ve) l),T.UNKNOWN,p) : A.texp
        | s te ve (ListExp (l,p)) = 
            A.ELIST (Vector.fromList (map (s te ve) l),T.UNKNOWN,p) : A.texp
      (*  | s te ve (LetExp (l,e,p)) = 
          let
             val (te',ve',dl) = convert te ve l
          in
             A.ELET (dl,s te' ve' e,T.UNKNOWN,p)
          end *)
        | s te ve (IfExp (a,b,c,p)) = 
             A.EIF (s te ve a, s te ve b, s te ve c, T.UNKNOWN, p)
        | s te ve (WhileExp (a,b,p)) = 
            A.EWHILE (s te ve a, s te ve b,T.UNKNOWN,p)
        | s te ve (CaseExp (a,m,p)) = 
          let
             val m' = Vector.fromList(map (fn (Match (a,b,_)) =>
                               let
                                  val (te',ve',a') = cpat te ve a
                               in (a', s te' ve' b) end) m)
          in
             A.ECASE (s te ve a, m', T.UNKNOWN, p)
          end
        | s te ve (FnExp ([Match (IdPat (Ident (n,_),_),e,_)],p)) = 
          let
             val ve' = Symbol.enter (ve,S.symbol n,Env.VarEntry (T.UNKNOWN))
          in
            A.EFN (S.symbol n, s te ve' e, T.FN(T.UNKNOWN,T.UNKNOWN), p)
          end
        | s te ve (FnExp (_,p)) = raise IllegalParseTree ("FnExp",p)
        | s te ve (HandleExp (e,l,p)) = raise Fail "handle not implemented" 
        | s te ve (Match (a,b,p)) = raise IllegalParseTree ("Match",p)
        | s te ve (Int (b,n,p)) = 
            A.ELIT(A.LINT ((if b then ~1 * (valOf (Int.fromString n))
                                else valOf (Int.fromString n)), p),T.t_int, p)
        | s te ve (Real _) = raise Fail "Real not impl."
        | s te ve (Char (n,p)) = 
            A.ELIT(A.LCHAR (String.sub (n,0),p),T.t_char,p)
        | s te ve (String (n,p)) =
            A.ELIT(A.LSTRING (n,p),T.t_string,p)
        | s te ve (UnitExp p) = A.ELIT(A.LUNIT p, T.t_unit, p)
        | s te ve (Ident (x,p)) = A.EID (Symbol.symbol x,T.UNKNOWN,p)
        | s te ve pt = raise Fail ("expression match failed in: " ^ pp pt)

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
                                 in (te'',ve'',r@[r']) end)
                                    (te,ve,[]) l
             
                 in (te',ve',se,A.DMUTUAL(Vector.fromList r, p)) end
          in
             prc l
          end
        | d te ve se (ValRec ([],p)) = raise IllegalParseTree ("ValRec[]",p)
        | d te ve se (ValRec ([b],p)) = 
          let
             val (te',ve',se',b') = d te ve se b
          in (te',ve',se,A.DVALREC (b',T.UNKNOWN,p)) end
        | d te ve se (ValRec (l,p)) =
          let
             val (te',ve',r) =  
                     List.foldl (fn (bind, (te',ve',r)) => 
                                 let
                                    val (te'',ve'',se'',r') = d te' ve' se bind
                                 in (te'',ve'',r@[r']) end)
                                    (te,ve,[]) l
             
          in (te',ve',se,A.DVALREC(A.DMUTUAL(Vector.fromList r, p),T.UNKNOWN,p)) end
        | d te ve se (TypeDec (l,p)) =
          let
             val (te',ve',_,_) = 
                      List.foldl (fn (b,(te',ve',se',_)) => d te' ve' se b) 
                        (te,ve,se,A.DNULL p) l
          in
             (te',ve',se,A.DNULL p)
          end
        | d te ve se (DatatypeAssign (a,b,p)) = 
               raise Fail "DatatypeAssign not implemented"
        | d te ve se (DatatypeDec (l,p)) =
          let
             val (te',ve',r) =  
                     List.foldl (fn (bind, (te',ve',r)) => 
                                 let
                                    val (te'',ve'',se'',r') = d te' ve' se bind
                                 in (te'',ve'',r@[r']) end)
                                    (te,ve,[]) l
             
          in (te',ve',se,A.DMUTUAL(Vector.fromList r,p)) end
        | d te ve se (FunDec (ll,p)) = raise IllegalParseTree ("FunDec",p)
        | d te ve se (ValBind (a,b,p)) =
          let
             val (te',ve',A.PID(s',_,_)) = 
               case (cpat te ve a) of (t,v,A.PID x) => (t,v,A.PID x)
                                    | _ => raise IllegalParseTree ("ValBind",p)
             val e = s te' ve' b
          in
             (te',ve',se,A.DVAL (s',e,T.UNKNOWN,p))
          end
        | d te ve se (TypeBind ([],a,b,p)) =  
          let
             val a' = case (t te ve a) of T.NAME n => n
                                        | _ => raise IllegalParseTree ("TypeBind",p)
             val b' = t te ve b
             val te' = Symbol.enter (te,a',b')
          in
             (te',ve,se,A.DNULL p)
          end

        | d te ve se (TypeBind (l,a,b,p)) = 
          let
             val l' = map (t te ve) l
             val a' = case (t te ve a) of T.NAME n => n
                                        | _ => raise IllegalParseTree ("TypeBind",p)
             val b' = T.CONSTR(l',t te ve b)
             val te' = Symbol.enter (te,a',b')
          in
             (te',ve,se,A.DNULL p)
          end
        | d te ve se (FunMatch (a,l,_,c,p)) =
             raise IllegalParseTree ("FunMatch",p)
        | d te ve se (DataBind (l,a,b,p)) =
             raise Fail "Databind not implemented"
        | d te ve se (ConBind (a,_,p)) = raise Fail "ConBind not implemented"
        | d te ve se (StructDec (l,p)) =  
          let
             val (te',ve',se',_) = 
               List.foldl (fn (b,(te',ve',se',_)) => d te' ve' se' b) 
                                                         (te,ve,se,A.DNULL p) l
          in
             (te',ve',se',A.DNULL p)
          end

        | d te ve se (StructBind (a,Structure (l,p1),p2)) =
          let
             val a' = case (s te ve a) of A.EID (n,t,p) => n
                                        | _ => raise Fail "non-id StructBind"
             val (te',ve',se',l') = convert te ve se l
             (* TODO: push the type and value environments into the struct env *)
          in
             (te,ve,se',A.DSTRUCTURE (a',Vector.fromList l',p1))
          end
        | d te ve se t = raise Fail "decl match"
   in
        List.foldl (fn (a,(te',ve',se',k)) =>
                     let
                        val (te'',ve'',se'',k') = d te' ve' se' a 
                     in
                        (te'',ve'',se'',k@[k'])
                     end) (tenv,venv,senv,[]) pt
   end
end

