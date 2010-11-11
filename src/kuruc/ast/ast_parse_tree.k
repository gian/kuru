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

   val strfresh = ref 0

   fun freshstrid () = (strfresh := 1 + !strfresh;
                        "_struct" ^ Int.toString (!strfresh))

   fun resolveParent se errorReport =
      let
         val _ = Debug.print Debug.debugmore
            "Chaining to parent to resolve." 
      in
         case S.look (se,S.symbol "__parent") of
            NONE => Error.fail errorReport
          | SOME (Env.ParentEntry se') => se'
      end

   fun resolveLongId te ve se l =
   let
      val path = ref [] : string list ref

      fun reportPath i = 
         if length (!path) > 0 then " in " ^ 
            (String.concatWith "." (!path)) ^ "." ^ i else
            ""

      fun resolveLongId' te ve se (l as LongId (Ident (s,pk),r,p)) = 
            (case (S.look (se,S.symbol s)) of
                NONE => (resolveLongId' te ve 
                         (resolveParent se
                           ("Unknown structure '" ^ s ^ "'" ^ 
                                       reportPath s,pk)) l)
              | SOME (Env.StructEntry (te',ve',se')) => 
                      (path := !path @ [s];
                        (Symbol.symbol s) :: 
                         resolveLongId' te' ve' se' r))
       | resolveLongId' te ve se (Ident (s,pk)) = 
            (case (S.look (ve,S.symbol s)) of
                NONE => Error.fail ("Unknown identifier '" ^ s ^ "'" ^ 
                                       reportPath s,pk)
              | SOME _ => [Symbol.symbol s])
       | resolveLongId' te ve se _ = raise Fail "Not implemented resolveLongId'"
   in
      resolveLongId' te ve se l
   end

   fun resolveTypeId te ve se l =
   let
      val path = ref [] : S.symbol list ref

      fun reportPath i = 
         if length (!path) > 0 then " in " ^ 
            (String.concatWith "." (map S.name (!path))) ^ "." ^ i else
            ""

      fun resolveLongId' te ve se (l as LongId (Ident (s,pk),r,p)) = 
            (case (S.look (se,S.symbol s)) of
                NONE => (resolveLongId' te ve 
                         (resolveParent se
                           ("Unknown structure '" ^ s ^ "'" ^ 
                                       reportPath s,pk)) l)
              | SOME (Env.StructEntry (te',ve',se')) => 
                      (path := !path @ [S.symbol s];
                         resolveLongId' te' ve' se' r))
       | resolveLongId' te ve se (Ident (s,pk)) = 
            (case (S.look (te,S.symbol s)) of
                NONE => Error.fail ("Unknown type identifier '" ^ s ^ "'" ^ 
                                       reportPath s,pk)
              | SOME t => (!path, t))
       | resolveLongId' te ve se _ = raise Fail "Not implemented resolveLongId'"
   in
      resolveLongId' te ve se l
   end

   fun resolveStructId te ve se l =
   let
      val path = ref [] : string list ref

      fun reportPath i = 
         if length (!path) > 0 then " in " ^ 
            (String.concatWith "." (!path)) ^ "." ^ i else
            ""

      fun resolveLongId' te ve se (l as LongId (Ident (s,pk),r,p)) = 
            (case (S.look (se,S.symbol s)) of
                NONE => (resolveLongId' te ve 
                         (resolveParent se
                           ("Unknown structure '" ^ s ^ "'" ^ 
                                       reportPath s,pk)) l)
              | SOME (Env.StructEntry (te',ve',se')) => 
                      (path := !path @ [s];
                        (Symbol.symbol s) :: 
                         resolveLongId' te' ve' se' r))
       | resolveLongId' te ve se (Ident (s,pk)) = 
            (case (S.look (se,S.symbol s)) of
                NONE => Error.fail ("Unknown structure identifier '" ^ s ^ "'" ^ 
                                       reportPath s,pk)
              | SOME _ => [Symbol.symbol s])
       | resolveLongId' te ve se _ = raise Fail "Not implemented resolveLongId'"
   in
      resolveLongId' te ve se l
   end

   fun symListToStruct p se [] = Error.fail ("Empty long-id in struct assign",p)
     | symListToStruct p se [h] = 
       (case (S.look (se,h)) of 
          SOME e => e
        | NONE => symListToStruct p (resolveParent se 
                      ("Unknown id " ^ S.name h ^ " in path.",p)) [h]) 
     | symListToStruct p se (h::t) = 
       (case (S.look (se,h)) of
          SOME (Env.StructEntry (_,_,e)) => symListToStruct p e t
        | NONE => symListToStruct p (resolveParent se 
                      ("Unknown id " ^ S.name h ^ " in path.",p)) (h::t))

   fun convert tenv venv senv pt = 
   let
      fun t te ve se (Var (Ident (a,_),p)) = 
            T.POLY ((Char.ord (String.sub (a,0)))-(Char.ord #"a"))
        | t te ve se (TyArrow (a,b,p)) = T.FN ([t te ve se a], t te ve se b)
        | t te ve se (TyPair (a,p)) = T.TUPLE (map (t te ve se) a) 
        | t te ve se (TyCon (a,b,p)) = T.CONSTR ([t te ve se a], t te ve se b)
        | t te ve se (TyName (a,p)) = 
          let
             val _ = Debug.print Debug.debug 
                        ("Converting TyName: " ^ prettyPrint a ^ "\n")

             val (path,a') = resolveTypeId te ve se a

             fun tr [] x = x
               | tr p (T.VAR a) = T.VAR a
               | tr p (T.FN (a,b)) = T.FN (map (tr p) a, tr p b)
               | tr p (T.TUPLE a) = T.TUPLE (map (tr p) a)
               | tr p (T.CONSTR (a,b)) = T.CONSTR (map (tr p) a, tr p b)
               | tr p (T.NAME a) = T.LNAME (p @ [a]) 
          in 
             tr path a'
          end
        | t te ve se (Ident (a,p)) = T.NAME (S.symbol a)
        | t te ve se n = Error.fail ("Unknown type not handled: " ^ 
                                    ParseTree.prettyPrint n,(0,""))

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

             val (i,t')
                   = (case t of 
                        T.CON (i,NONE,ty) => (i,ty)
                      | T.CON (i,SOME _,ty) =>
                           Error.fail ("Constructor expects argument",p) 
                      | _ => Error.fail ("Non-constructor in pattern",p)) 
          in
             (te,ve,A.PCONSTR (i,NONE,t',p))
          end
        | cpat te ve se (ConstrPat (Ident (n,_),SOME b,p)) = 
          let
             val s' = S.symbol n 
             val t = case (S.look (ve,s')) of
                        SOME (Env.VarEntry t') => t'
                      | NONE => Error.fail ("Unrecognised constructor: " ^ n,p)
             val (te',ve',b') = cpat te ve se b

             val (i,t',t'')
                   = (case t of 
                        T.CON (i,NONE,ty) => 
                           Error.fail ("Constructor given unexpected argument",p)
                      | T.CON (i,SOME ty1,ty2) => (i,ty2,ty1)
                      | _ => Error.fail 
                           ("Non-constructor in pattern", p))
          in
             (te',ve',A.PCONSTR (i,SOME (b',t''),t',p))
          end
        | cpat te ve se (TyAnnPat (a,b,p)) = raise Fail "TyAnnPat not impl"
        | cpat te ve se (IdPat (Ident (n,_),p)) =
          (case S.look (ve,S.symbol n) of
            SOME (Env.VarEntry (T.CON (i,NONE,t))) =>
            cpat te ve se (ConstrPat (Ident (n,p),NONE,p))
          | SOME (Env.VarEntry (T.CON (i,SOME _,t))) =>
            Error.fail ("Using constructor pattern '" ^ n ^ "' without arguments", 
                        p)
          | _ => let
               val s' = S.symbol n
               val ve' = S.enter (ve,s',Env.VarEntry T.UNKNOWN) 
            in
               (te,ve',A.PID (s',T.UNKNOWN,p))
            end)
        | cpat te ve se (IdPat (_,p)) = raise IllegalParseTree ("IdPat",p)
        | cpat te ve se (WildcardPat p) = 
            (te,ve,A.PWILD p)
        | cpat te ve se (NilPat p) = (te,ve,A.PNIL p)
        | cpat te ve se (UnitPat p) = (te,ve,A.PUNIT p)
        | cpat te ve se p = 
            raise IllegalParseTree ("cpat Match: " ^ prettyPrint p,(0,""))

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
            raise IllegalParseTree ("ast_parse_tree: ListExp node remains!",p)
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
                                               raise IllegalParseTree  
                                                ("non-Match in case expression",p))
                                                m)
          in
             A.ECASE (s te ve se a, m', T.UNKNOWN, p)
          end
        | s te ve se (FnExp ([Match (IdPat (Ident (n,_),_),e,_)],p)) = 
          let
             val ve' = Symbol.enter (ve,S.symbol n,Env.VarEntry (T.UNKNOWN))
          in
            A.EFN ([S.symbol n], s te ve' se e, T.UNKNOWN, p)
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
        | s te ve se (Ident (x,p)) = 
          (case S.look (ve,Symbol.symbol x) of 
             NONE => Error.fail ("Unknown identifier '" ^ x ^ "'",p)
           | SOME (Env.VarEntry t) => A.EID (Symbol.symbol x,t,p))
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
        | d te ve se (ValDesc (l,p)) =
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
        | d te ve se (TypeDesc (l,p)) =
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
             
          in 
            (te',ve',se,r)
          end
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
        | d te ve se (ValDescBind (a,b,p)) =
          let
             val (te',ve',(s',_,_)) = 
               case (cpat te ve se a) of (t,v,A.PID x) => (t,v,x)
                                    | _ => raise IllegalParseTree ("ValBind",p)
             val e = t te ve se b

             val ve' = S.enter (ve,s',Env.VarEntry e)
          in
             (te',ve',se,[A.DVAL (s',A.ENIL p,e,p)])
          end
        | d te ve se (TypeBind ([],TyName (Ident(a,_),_),b,p)) =  
          let
             val _ = Debug.print Debug.internal 
               ("Type binding: " ^ a ^ " = " ^ prettyPrint b ^ "\n")

             val b' = t te ve se b
             
             val _ = Debug.print Debug.internal 
               ("Type binding resolved to: " ^ a ^ " = " ^ T.prettyPrint b' ^ "\n")

             val te' = Symbol.enter (te,S.symbol a,b')
          in
             (te',ve,se,[])
          end
        | d te ve se (TypeBind (l,a,b,p)) =
            raise Fail "ast-parse-tree: TypeBind not fully implemented"
        | d te ve se (TypeDescBind ([],a,p)) =  
          let
             val a' = case (t te ve se a) of T.NAME n => n
                         | _ => raise IllegalParseTree ("TypeDescBind",p)
             val b' = T.UNKNOWN 
             val te' = Symbol.enter (te,a',b')
          in
             (te',ve,se,[])
          end
        | d te ve se (TypeDescBind (l,a,p)) = 
          let
             val l' = map (t te ve se) l
             val a' = case (t te ve se a) of T.NAME n => n
                         | _ => raise IllegalParseTree ("TypeDescBind",p)
             val b' = T.CONSTR(l',T.UNKNOWN)
             val te' = Symbol.enter (te,a',b')
          in
             (te',ve,se,[])
          end
        | d te ve se (InfixDec _) = (te, ve, se, [])
        | d te ve se (InfixrDec _) = (te, ve, se, [])
        | d te ve se (FunMatch (a,l,_,c,p)) =
             raise IllegalParseTree ("FunMatch",p)
        | d te ve se (DataBind (l,Ident (a,p1),b,p)) =
          let
             val a' = S.symbol a

             val te' = S.enter (te,a',T.NAME a')
             val cbi = ref 0
             val cons = ref [] : T.ty list ref

             val (te'',ve',ins) = 
                List.foldl (fn (cb, (te,ve,ins)) =>
                   let
                      val (te',ve',ins') = 
                        (case cb of
                           ConBind (Ident (id,pos),NONE,_) =>
                              let
                                 val ve' = S.enter (ve,S.symbol id,
                                                      Env.VarEntry (T.CON (
                                                                     !cbi,
                                                                     NONE,
                                                                     T.NAME a')))
                                 val _ = cons := !cons @ [T.NAME (S.symbol id)]
                                 
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
                                           Env.VarEntry (T.CON (!cbi,
                                                                SOME f',
                                                                T.NAME a')))

                              val _ = cons := !cons @ 
                                    [T.FN ([f'],T.NAME (S.symbol id))]

                              val p = S.symbol ("_p" ^ Int.toString (!cbi)) 
                              val ins' = [A.DVAL (S.symbol id,
                                             A.EFN ([p],
                                               A.ECON (!cbi,
                                                SOME (A.EID (p,f',pos),
                                                      f'),
                                                T.NAME a',
                                                pos),
                                             T.FN ([f'],T.NAME a'),pos),
                                             T.FN ([f'],T.NAME a'),pos)]
                              val _ = cbi := !cbi + 1 
                           in
                               (te',ve',ins')
                           end
                         | _ => Error.fail ("ast-parse-tree: Non-ConBind?",p))
                   in
                      (te',ve',ins @ ins')
                   end) (te',ve,[]) b

             val te''' = S.enter (te'',a',T.UNION (a',!cons))
          in
             (te''',ve',se,ins)
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
        | d te ve se (StructDesc ([b],p)) =  
          let
             val (te',ve',se',b') = d te ve se b
          in
             (te,ve,se',b')
          end
        | d te ve se (StructDesc (l,p)) =
          let
             val (te',ve',se',l') = convert te ve se l
          in
             (te,ve,se',[A.DMUTUAL (Vector.fromList l',p)])
          end
        | d te ve se (StructBind (Ident(a,p),c,Structure (l,p1),p2)) =
          let
             val a' = S.symbol a

             val separent = S.enter(S.empty,S.symbol "__parent",
                                       Env.ParentEntry se)

             val (te',ve',se',l') = convert te ve separent l

             val se'' = S.enter (se,a',Env.StructEntry (te',ve',se'))

             val (se''',c',si) = 
               case c of Implicit => (se'',NONE,[])
                       | Ascribe (Ident (sy,p)) => (se'',SOME (S.symbol sy),[])
                       | Ascribe (Signature x) =>
                         let
                            val sid = freshstrid ()
                            val ns = SigBind (Ident (sid,p1),Signature x,p1)
                            val (_,_,ss,si) = d te ve se ns
                         in
                            (ss,SOME (S.symbol sid),si)
                         end
          in
             (te,ve,se''',si @ [A.DSTRUCTURE (a',c',Vector.fromList l',p1)])
          end
        | d te ve se (StructBind (Ident (a,p1),c,b,p)) =
          let
             val a' = S.symbol a

             val b' = resolveStructId te ve se b
             val b'' = symListToStruct p se b'

             val b''' = case b' of [h] => A.EID (h,T.UNKNOWN,p)
                                 | l => A.ELID (l,T.UNKNOWN,p)

             val se' = case S.look (se,S.symbol "__parent") of
                           NONE => Error.bug ("AstParseTree",
                                              "Cannot find __parent in struct",p)
                         | SOME (Env.ParentEntry se) => se
                         | _ => Error.bug ("AstParseTree",
                                           "Non-ParentEntry in __parent",p)

             val se'' = S.enter (se',a',b'')
          in
             (te,ve,se'',[A.DSTRUCTASSIGN (a',b''',p)])
          end
        | d te ve se (SigDec ([s],p)) = 
            d te ve se s
        | d te ve se (SigDec (s,p)) =
          let
             val (te',ve',se',s') =
               List.foldl (fn (s,(te,ve,se,r)) => 
                     let
                        val (te',ve',se',r') = d te ve se s
                     in
                        (te,ve,se',r@r')
                     end) (te,ve,se,[]) s
          in
             (te,ve,se',[A.DMUTUAL (Vector.fromList s',p)])
          end
        | d te ve se (SigBind (a,Signature (l,_),p)) = 
          let
             val a' = case (s te ve se a) of A.EID (n,t,p) => n
                                        | _ => Error.fail("non-id SigBind",p)
             val (te',ve',se',l') = convert te S.empty se l

             val se'' = S.enter (se,a',Env.SigEntry (te',ve',se'))
          in
             (te,ve,se'',[A.DSIGNATURE (a',Vector.fromList l',p)])
          end
        | d te ve se (ExnDec (a,x,p)) =
            (* We've already translated this to a datatype in ast-promote-exns *)
             (te,ve,se,[])
        | d te ve se (OpenDec (a,p)) =
          let
             fun sb se [] = Error.fail ("Empty long-id in 'open'",p)
               | sb se [h] = (case (S.look (se,h)) of 
                                 (SOME e) => e
                               | NONE => 
                               Error.fail("Invalid id in 'open'",p)) 
               | sb se (h::t) = (case (S.look (se,h)) of
                                  (SOME (Env.StructEntry (_,_,e))) => sb e t
                                 | NONE => 
                               Error.fail("Invalid id in 'open'",p)) 

             val a' = s te ve se a
             val a'' = case a' of A.EID (n,t,p') => sb se [n]
                                | A.ELID (nl,t,p') => sb se nl

             val (ste,sve,sse) = 
               case a'' of
                  Env.StructEntry s => s
                | _ => Error.fail ("Attempted to open non-structure.",p)

             val tkeys = 
                     map (fn (k,v) => (S.unhash k, v)) (S.keys ste)
             val vkeys = 
                     map (fn (k,v) => (S.unhash k, v)) (S.keys sve)
             val skeys = 
                     map (fn (k,v) => (S.unhash k, v)) (S.keys sse)

             val _ = List.app (Debug.print Debug.debug)
                     (map (fn (SOME k,v) => 
                              "opening structure: " ^ prettyPrint a ^ ": type "
                              ^ S.name k ^ 
                              " = " ^ T.prettyPrint v ^ "\n"
                            | _ => "Unknown structure element\n") tkeys)

             val _ = List.app (Debug.print Debug.debug)
                     (map (fn (SOME k,Env.VarEntry v) => 
                              "opening structure: " ^ prettyPrint a ^ ": val "
                              ^ S.name k ^ 
                              " : " ^ T.prettyPrint v ^ "\n"
                            | _ => "Unknown structure element\n") vkeys)

             val _ = List.app (Debug.print Debug.debug)
                     (map (fn (SOME k,v) => 
                              "opening structure: " ^ prettyPrint a ^ ": structure "
                              ^ S.name k ^ "\n" 
                            | _ => "Unknown structure element\n") skeys)
            
             val te' =
               List.foldl (fn ((SOME k,t),te) =>
                                  S.enter (te,k,t)
                            | ((NONE,_),_) => te) te tkeys
             val ve' =
               List.foldl (fn ((SOME k,t),ve) =>
                                  S.enter (ve,k,t)
                            | ((NONE,_),_) => ve) ve vkeys
             
             val se' =
               List.foldl (fn ((SOME k,t),se) =>
                                  S.enter (se,k,t)
                            | ((NONE,_),_) => se) se skeys
          in
             (te',ve',se',[A.DOPEN(a',p)])
          end
        | d te ve se (Import (Ident(i1,p1),tt,Ident(i2,p2),p)) =
          let
             val t' = t te ve se tt

             val i1' = S.symbol i1

             val i2' = S.symbol i2

             val ve' = S.enter(ve,i2',Env.VarEntry t')
          in
             (te,ve',se,[A.DIMPORT (i1',t',i2',p)])
          end
        | d te ve se t = raise Fail "ast-parse-tree.k: decl match"
   in
        List.foldl (fn (a,(te',ve',se',k)) =>
                     let
                        val (te'',ve'',se'',k') = 
                           d te' ve' se' a handle CompileError => (te',ve',se',[])
                     in
                        (te'',ve'',se'',k@k')
                     end) (tenv,venv,senv,[]) pt
   end handle IllegalParseTree (m,p) => 
         Error.fail ("IllegalParseTree: " ^ m ^ 
                     ".  This is likely a compiler bug.",p) 
end

