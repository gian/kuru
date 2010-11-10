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

   datatype structentry = StructEntry of T.ty S.table *  
                                         I.exp S.table * 
                                         structentry S.table

   datatype fragment = 
      FSTRING of Temp.label * string
    | FFN of Temp.label * I.exp list * I.stm list * Temp.ty
    | FALIAS of I.exp * I.exp
    | FPOLYFN of Temp.label * S.symbol list * A.texp * Temp.ty 

   val fragments = ref [] : fragment list ref

   fun add_fragment f = fragments := !fragments @ [f]
   fun add_fragments f = fragments := !fragments @ f

   fun get_fragment l = 
      let
         fun fr ((f as FPOLYFN (s,_,_,_))::t) = if s = l then SOME f else fr t
           | fr ((FALIAS (I.NAME a, I.NAME b))::t) = 
               if a = l then get_fragment b else fr t
           | fr (x::t) = fr t
           | fr [] = NONE
      in
         fr (!fragments)
      end
   
   fun reset_fragments () = fragments := []

   fun printFragment (FSTRING (l,s)) = 
      Temp.prettyPrint l ^ ": \"" ^ s ^ "\""
     | printFragment (FFN (l,el,sl,t)) =
      "function " ^ Temp.prettyPrint l ^ " (" ^
         String.concatWith "," (map I.ppe el) ^
       ") {\n   " ^
         String.concatWith "\n   " (map I.ppstm sl) ^ "\n}\n"
     | printFragment (FALIAS _) = "FALIAS\n"
     | printFragment (FPOLYFN (l,s,e,_)) = "FPOLYFN " ^ Temp.prettyPrint l ^
                                           " = " ^ A.ppe e ^ "\n"

   fun printFragments [] = ""
     | printFragments (h::t) = printFragment h ^ "\n" ^ 
                                 printFragments t

   fun is_simple (I.NAME _) = true
     | is_simple (I.TEMP _) = true
     | is_simple (I.CONST _) = true
     | is_simple (I.BCONST _) = true
     | is_simple (I.UNDEF _) = true
     | is_simple (I.VOID) = true
     | is_simple (I.NULL _) = true
     | is_simple _ = false

   (* "Contingent label" - used to prevent adding of labels 
      to create empty blocks. *)
   fun cont_lab ([],_) = []
     | cont_lab (x,l) = l

   fun tr_ty te (T.NAME s) = 
      let
         val s' = Symbol.name s
      in
         (case s' of 
            "int" => Temp.getMachineWord ()
          | "real" => Temp.r64
          | "char" => Temp.i32
          | "string" => Temp.ptr (Temp.aggr 
                           [Temp.getMachineWord (), Temp.ptr (Temp.i8)])
          | "bool" => Temp.i1
          | "unit" => Temp.ptr (Temp.i8)
          | "cvoid" => Temp.void
          | x => (case S.look (te,s) of
                     NONE => raise Fail "tr_ty: unhandled NAME type"
                   | SOME t => tr_ty te t
                 )
          )
      end
     | tr_ty te (T.FN (a,b)) =
         Temp.fn_ptr (map (tr_ty te) a, tr_ty te b)
     | tr_ty te (T.CONSTR ([t1],t2)) = 
         (case t2 of (T.NAME n) => 
            (case S.name n of 
               "list" => (Temp.ptr (Temp.aggr [
                                      Temp.ptr (Temp.i8),
                                      Temp.ptr (Temp.self_ref 2)]))
             | "_opaque" => (Temp.ptr Temp.i8)
             | _ => raise Fail "tr_ty: CONSTR not impl.")
            | _ => raise Fail "tr_ty: Invalid CONSTR type.")
     | tr_ty te (T.TUPLE l) = Temp.aggr (map (tr_ty te) l)
     | tr_ty te (T.UNION _) = Temp.ptr (Temp.aggr [Temp.i32, Temp.ptr Temp.i8])
     | tr_ty te (T.VAR _) = Temp.ptr (Temp.i8)
     | tr_ty te (T.META _) = Temp.ptr (Temp.i8)
     | tr_ty te (T.POLY _) = Temp.ptr (Temp.i8)
     | tr_ty te t = raise Fail ("tr_ty: unhandled type: " ^ T.prettyPrint t)

   fun tr_val (A.LINT (v,p)) =   (I.CONST v,[]) (* FIXME: negative numbers? *) 
     | tr_val (A.LREAL (v,p)) =  (I.RCONST v,[])
     | tr_val (A.LCHAR (v,p)) =  (I.CONST (Char.ord v),[])
     | tr_val (A.LSTRING (v,p)) =
       let
          val strty = Temp.aggr [Temp.getMachineWord (),Temp.ptr Temp.i8]
          val n = Temp.newglobal (Temp.ptr strty)
          val _ = add_fragment (FSTRING (n, v))
          val m = I.ELEM_PTR (Temp.array (size v + 1, Temp.i8),I.NAME n,0)
          val t0 = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val t1 = I.TEMP (Temp.newtemp (strty))
          val t2 = I.TEMP (Temp.newtemp (strty))
          val t3 = I.TEMP (Temp.newtemp (Temp.ptr strty))
          val m' = [I.MOVE (t0,m),
                    I.MOVE (t1,I.TUPLE_INS(I.UNDEF strty, I.CONST (size v + 1),0)),
                    I.MOVE (t2,I.TUPLE_INS (t1, t0, 1)),
                    I.MOVE (t3,I.ALLOCA (strty)),
                    I.STORE (t2,t3)]
       in
          (t3, m')
       end
     | tr_val (A.LUNIT p) = (I.NULL (Temp.ptr Temp.i8),[]) 
     | tr_val _ = raise Fail "tr_val: Not implemented"

   fun tr_pat te ve se (A.PCONST (A.ELIT (v,t,_),_,_)) =
      (fn (c,_,f) =>
       let
          val (v',ins) = tr_val v
          val t = Temp.newlabel ()
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
                  [I.MOVE (I.TEMP k,I.kstreq c v'),
                   I.CJUMP2 (I.NEQ,
                            I.NAME k,
                            I.CONST 1,
                            t,
                            f)]
               end)
          | A.LUNIT _ => ([I.JUMP (I.NAME t,[t])])
       in
          (te,ve,se,ins@v''@[I.LABEL t])
       end)
     | tr_pat te ve se (A.PCONST (_,_,p)) = 
            raise Fail ("tr_pat: Invalid constant pattern!")
     | tr_pat te ve se (A.PLIST (v,t,p)) = 
            raise Fail ("tr_pat: List pat not implemented") 
     | tr_pat te ve se (A.PTUPLE (v,T.TUPLE types,p)) =
       (fn (c,t,f) =>
         let
            fun mkt (c,t,f) =  
               Vector.foldli (fn (i,e,(te,ve,se,l)) =>
               let
                  val nlab = Temp.newlabel ()
                  val f' = tr_pat te ve se e
                  val rt = Temp.newtemp (tr_ty te (List.nth (types,i)))
                  val l' = l @ [I.MOVE (I.TEMP rt,I.TUPLE_IND(c,i))]
                  val (te',ve',se',l'') = (f' (I.TEMP rt,nlab,f))
                  val nl = if length l'' = 0 then [] else [I.LABEL nlab]
               in
                  (te',ve',se',l'@l''@nl)
               end) (te,ve,se,[]) v

            val (te',ve',se',fl) = mkt (c,t,f) 
         in
            (te',ve',se',fl)
         end)
     | tr_pat te ve se (A.PCONSTR (i,NONE,t,p)) = 
       (fn (c,t,f) =>
       let
          val conty = Temp.aggr [Temp.i32, Temp.ptr Temp.i8]
          val tmp = I.TEMP (Temp.newtemp conty)
          val tmp2 = I.TEMP (Temp.newtemp Temp.i32)
          val instr = [I.MOVE(tmp,I.LOAD c),
                       I.MOVE(tmp2,I.TUPLE_IND (tmp,0)),
                       I.CJUMP2(I.EQ,tmp2,I.CONST i,t,f)
                       ]
       in
          (te,ve,se,instr)
       end)
     | tr_pat te ve se (A.PCONSTR (i,SOME (pat,t1),t2,p)) = 
       (fn (c,tr,f) =>
       let
          val t = Temp.newlabel ()
          val conty = Temp.aggr [Temp.i32, Temp.ptr Temp.i8]
          val tmp = I.TEMP (Temp.newtemp conty)
          val tmp2 = I.TEMP (Temp.newtemp Temp.i32)
          val tmp3 = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val tmp3' = I.TEMP (Temp.newtemp (Temp.ptr (tr_ty te t1)))
          val tmp3'' = I.TEMP (Temp.newtemp (tr_ty te t1))
          val instr = [I.MOVE(tmp,I.LOAD c),
                       I.MOVE(tmp2,I.TUPLE_IND (tmp,0)),
                       I.CJUMP2(I.EQ,tmp2,I.CONST i,t,f),
                       I.LABEL t,
                       I.MOVE(tmp3,I.TUPLE_IND (tmp,1)),
                       I.MOVE(tmp3',I.BITCAST (tmp3,Temp.ptr(tr_ty te t1))),
                       I.MOVE(tmp3'',I.LOAD tmp3')]
          val f' = tr_pat te ve se pat
          val t2 = Temp.newlabel ()
          val (te',ve',se',inst2) = f' (tmp3'',t2,f)
       in
          (te,ve',se,instr @ inst2 @ [I.JUMP (I.NAME tr,[tr])])
       end)
     | tr_pat te ve se (A.PID (s,ty,p)) =
       (fn (c,t,f) =>
       let
          val ts = if is_simple c then c else I.TEMP (Temp.newtemp (tr_ty te ty))
          val ve' = S.enter (ve, s, ts)
          val mv = if is_simple c then [] else [I.MOVE (ts,c)]
       in
         (te,ve',se,mv)
       end)
     | tr_pat te ve se (A.PINFIX (a, b, r, T.CONSTR ([t1],t2),p2)) =
       (fn (c,t,f) =>
       let
          (* FIXME: Check that t2 is t_list *)
          val _ = case (S.name b) of 
                     "::" => ()
                   | _ => raise Fail ("tr_pat: Invalid pattern infix operator!")

          (* FIXME: Output check for empty list. *)
          
          val ht = tr_ty te t1
          val h0 = I.TEMP (Temp.newtemp (Temp.aggr [Temp.ptr Temp.i8,
                                                    Temp.ptr (Temp.self_ref 2)]))
          val h1 = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val h1' = I.TEMP (Temp.newtemp (Temp.ptr ht))
          val h2 = I.TEMP (Temp.newtemp (ht))
          val h3 = I.TEMP (Temp.newtemp (Temp.ptr (
                                           Temp.aggr [Temp.ptr Temp.i8,
                                                      Temp.ptr (Temp.self_ref 2)
                                                     ]
                                        )))
          val headc = [I.MOVE (h0, I.LOAD c),
                       I.MOVE (h1, I.TUPLE_IND (h0,0)),
                       I.MOVE (h1', I.BITCAST (h1, Temp.ptr (ht))),
                       I.MOVE (h2, I.LOAD h1'),
                       I.MOVE (h3, I.TUPLE_IND (h0,1))]

          val fh = tr_pat te ve se a
          val (te',ve',se',a') = fh (h2,t,f)

          val ft = tr_pat te ve' se r
          val (te',ve'',se',b') = ft (h3,t,f)
       in
          (te',ve'',se,headc @ a' @ b' @ [I.JUMP (I.NAME t, [t])])
       end)
     | tr_pat te ve se (A.PWILD p) =
       (fn (c,t,f) =>
            (te,ve,se,[])
       )
     | tr_pat te ve se (A.PNIL p) =
       (fn (c,t,f) =>
            (te,ve,se,
             [I.CJUMP2 (I.EQ,c,I.NULL (Temp.ptr (Temp.aggr [Temp.ptr Temp.i8,
                                                            Temp.ptr (
                                                               Temp.self_ref 2
                                                            )])),t,f)]))
     | tr_pat te ve se _ = raise Fail "tr_pat: Not implemented"

   and tr_exp te ve se (A.ELIT (v,t,p)) = 
       let 
          val (v',ins) = tr_val v
       in
          (te,ve,se,v',ins)
       end
	  | tr_exp te ve se (A.EID (s,t,p)) = 
         (case S.look (ve,s) of NONE => 
               raise Fail ("Unknown EID "^S.name s^ " in tr_exp")
                              | SOME tm => (te,ve,se,tm,[]))
	  | tr_exp te ve se (A.ELID (l,t,p)) = 
       let
          fun res vtbl stbl [] = raise Fail "tr_exp: BUG: empty long id?"
            | res vtbl stbl [id] = (case S.look (vtbl,id) of
                               NONE => raise Fail "tr_exp: BUG: unknown id?" 
                             | SOME k => k)
            | res vtbl stbl (h::rest) = 
                                   (case S.look (stbl,h) of
                               NONE => 
                                 raise Fail "tr_exp: BUG: unknown str id?"
                             | SOME (StructEntry (te',ve',se')) =>
                                       res ve' se' rest)
          val t' = res ve se l
       in
          (te,ve,se,t',[]) 
       end
     | tr_exp te ve se (A.ETUPLE (v,T.TUPLE t,p)) = 
       let
          val (instr,rt) = 
            Vector.foldli (fn (i,a,(c',rt)) =>
               let
                  val (te',ve',se',a',c) = tr_exp te ve se a
                  val rt' = Temp.newtemp (tr_ty te (T.TUPLE t))
                  val ins = [I.MOVE(I.TEMP rt', I.TUPLE_INS (rt,a',i))]
               in
                  (c' @ c @ ins,I.TEMP rt')
               end) ([],I.UNDEF (tr_ty te (T.TUPLE t))) v
       in
          (te,ve,se,rt,instr) 
       end
     | tr_exp te ve se (A.ENIL p) = 
          (te,ve,se,I.NULL (Temp.ptr (Temp.aggr [Temp.ptr Temp.i8,
                                                            Temp.ptr (
                                                               Temp.self_ref 2
                                                            )])),[])
     | tr_exp te ve se (A.EIF (e1,e2,e3,t,p)) =
       let
          val (_,_,_,r1,in1) = tr_exp te ve se e1

          val tlab = Temp.newlabel ()
          val (_,_,_,r2,in2) = tr_exp te ve se e2

          val flab = Temp.newlabel ()
          val (_,_,_,r3,in3) = tr_exp te ve se e3

          val rlab = Temp.newlabel ()

          val rv = I.TEMP (Temp.newtemp (tr_ty te t))
          val phi = [I.LABEL rlab, I.PHI (rv,tr_ty te t,[(r2,tlab),(r3,flab)])]
       in
          (te,ve,se,rv,
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
     | tr_exp te ve se (A.EWHILE (c,e,t,p)) = raise Fail "tr_exp tewhile not impl." 
     | tr_exp te ve se (f as A.EFN (s,e,T.FN(it,rt),p)) =
       let
          val fid = Temp.newglobal (Temp.fn_ptr (map (tr_ty te) it,tr_ty te rt))
          val (p',ve') =
          List.foldl (fn ((s,t),(ts,ve)) =>
               let
                  val p = I.TEMP (Temp.newtemp (tr_ty te t))
                  val v' = S.enter (ve,s,p)
               in
                  (ts@[p],v')
               end) ([],ve) (ListPair.zip (s,it))
          
          val (te',ve'',se',e',instr) = tr_exp te ve' se e
          val _ = if Polymorphism.is_polymorphic f then
                                    (Debug.print Debug.debug "is_poly EFN\n";
                                    add_fragment (FPOLYFN (fid, s, f, 
                                                           tr_ty te 
                                                            (T.FN(it,rt)))))
                                    else
                                    add_fragment (FFN (fid, p',
                                            [I.LABEL (Temp.newlabel ())] @
                                                   instr @ 
                                                   [I.RET e'],tr_ty te rt))
       in
          (te,ve,se,I.NAME fid, [])
       end
     | tr_exp te ve se (A.ECASE (e,v,t,p)) =
       let
          val (_,_,_,e',in1) = tr_exp te ve se e

          val el = Temp.newlabel ()
          val fst = Temp.newlabel ()

          val rt = I.TEMP (Temp.newtemp (Temp.ptr (tr_ty te t)))
          
          val (instr,labs,_) =
            Vector.foldl (
               fn ((p,e),(ins,labs,lst)) =>
               let
                  val tlab = Temp.newlabel ()
                  val flab = Temp.newlabel ()
                  
                  val (te',ve',se',p') = (tr_pat te ve se p) (e',tlab,flab)
                  val (_,_,_,e'',eins) = tr_exp te ve' se' e
               in
                  (ins @ 
                    p' @ 
                    eins @
                    [I.STORE (e'',rt)] @
                    [I.JUMP (I.NAME el,[el])] @
                    [I.LABEL flab],
                  labs @ [(e'',lst)],flab)
               end) ([],[],fst) v

          val ret = I.TEMP (Temp.newtemp (tr_ty te t))

          val instr' = in1 @
                       [I.MOVE (rt, I.ALLOCA (tr_ty te t))] @
                       [I.JUMP (I.NAME fst,[fst])] @
                       [I.LABEL fst] @
                       instr @
                        [I.UNWIND] @
                        [I.LABEL el] @ 
                         [I.MOVE (ret,I.LOAD rt)]

       in
          (te,ve,se,ret,instr')
       end
     | tr_exp te ve se (A.EAPP(A.EID((r as "+",_),t1,p1),A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end
     | tr_exp te ve se (A.EAPP(A.EID((r as "-",_),t1,p1),A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end
     | tr_exp te ve se (A.EAPP(A.EID((r as "*",_),t1,p1),A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end
     | tr_exp te ve se (A.EAPP(A.EID((r as "/",_),t1,p1),A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end
     | tr_exp te ve se (A.EAPP(A.EID((r as "=",_),t1,p1),A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end
      | tr_exp te ve se (A.EAPP(A.EID((r as "<>",_),t1,p1),
                      A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end    
      | tr_exp te ve se (A.EAPP(A.EID((r as "<",_),t1,p1),A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end    
      | tr_exp te ve se (A.EAPP(A.EID((r as ">",_),t1,p1),A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end    
      | tr_exp te ve se (A.EAPP(A.EID((r as "<=",_),t1,p1),
         A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end    
      | tr_exp te ve se (A.EAPP(A.EID((r as ">=",_),t1,p1),
         A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end    
      | tr_exp te ve se (A.EAPP(A.EID((r as "_andalso",_),t1,p1),
         A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end    
      | tr_exp te ve se (A.EAPP(A.EID((r as "_orelse",_),t1,p1),
         A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)
          val ins = ins1 @ ins2 @ [I.MOVE (I.TEMP rt, I.BINOP (r,a,b))]
       in
          (te,ve,se,I.TEMP rt,ins)
       end  
     | tr_exp te ve se (A.EAPP(A.EID((r as "::",_),t1,p1),
          A.ETUPLE (v,t2,p2),t3,p3)) =
       let
          val (lhst,rhst) = (case t2 of T.TUPLE [a,b] => (tr_ty te a, tr_ty te b)
                                     | _ => raise Fail
                                       "tr_exp: Invalid operands to ::")

          val (_,_,_,a,ins1) = tr_exp te ve se (Vector.sub (v,0))
          val (_,_,_,b,ins2) = tr_exp te ve se (Vector.sub (v,1))
          val rt = Temp.newtemp (tr_ty te t3)

          val listty = Temp.aggr [Temp.ptr Temp.i8,Temp.ptr (Temp.self_ref 2)]
          val t0 = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val t0' = I.TEMP (Temp.newtemp (Temp.ptr lhst))
          val t1 = I.TEMP (Temp.newtemp (listty))
          val t2 = I.TEMP (Temp.newtemp (listty))
          val t3 = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val t3' = I.TEMP (Temp.newtemp (Temp.ptr listty))
          val m' = [I.MOVE (t0,I.MALLOC (lhst,1)),
                    I.MOVE (t0',I.BITCAST (t0,Temp.ptr lhst)),
                    I.STORE (a,t0'),
                    I.MOVE (t1,I.TUPLE_INS(I.UNDEF listty, t0, 0)),
                    I.MOVE (t2,I.TUPLE_INS (t1, b, 1)),
                    I.MOVE (t3,I.MALLOC (listty,1)),
                    I.MOVE (t3',I.BITCAST (t3,Temp.ptr listty)),
                    I.STORE (t2,t3')]

          val ins = ins1 @ ins2 @ m' 
       in
          (te,ve,se,t3',ins)
       end
       | tr_exp te ve se (A.EAPP (a,b,t,p)) = 
       let


          val _ = Debug.print Debug.debug ("tr_exp: is_polymorphic: " ^
                     A.ppe a ^ " : " ^ 
                     T.prettyPrint (A.getTypeExp a) ^ "\n") 

          val (_,_,_,b',instr1) = tr_exp te ve se b
          val (_,_,_,a',instr2) = tr_exp te ve se a

          val (a'',instr2')
         (*   = if Polymorphism.is_polymorphic a *)
              = if false
                     then 
                        let
                           val lbl = case a' of 
                                         I.NAME l => l
                                       | I.TEMP t => t
                                       | _ => raise Fail 
                                 ("tr_exp: BUG: Monomorphisation: " ^ 
                                  I.ppe a')


                           val _ = Debug.print Debug.debug
                              ("Monomorphising label: "^Temp.prettyPrint lbl ^"\n")
                           val frag = get_fragment lbl

                           val _ = Debug.print Debug.debug
                              ("Fragments:\n" ^ printFragments (!fragments) ^ "\n")

                           val fr = case frag of 
                              SOME (FPOLYFN (l,s,exp,_)) => exp
                            | SOME _ => raise Fail "tr_exp: BUG: Monomorphisation2"
                            | NONE => raise Fail "tr_exp: BUG: Monomorphisation3"
                                       
                           val mon = 
                              Polymorphism.monomorphise (A.getTypeExp a) 
                                                        (A.getTypeExp b) fr

                           val (_,_,_,m',ins') = tr_exp te ve se mon
                        in
                           (m',ins')
                        end
                     else (a',instr2)

          val rt = Temp.newtemp (tr_ty te t)
          
          val c = I.CALL (tr_ty te t,a'',b')
       in
          if tr_ty te t = Temp.void then 
             (te,ve,se,I.VOID,instr1 @ instr2 @ [I.EXP c])
          else
             (te,ve,se,I.TEMP rt,instr1 @ instr2 @ [I.MOVE (I.TEMP rt,c)])
       end
     | tr_exp te ve se (A.EHANDLE (pa,e,t,p)) = raise Fail "tr_exp: EHANDLE"
     | tr_exp te ve se (A.ESEQ (v,t,p)) =
       let
          val (rt,v') =
            Vector.foldl (
                fn (e,(rt,v)) =>
                  let
                     val (_,_,_,rt',instr) = tr_exp te ve se e
                  in
                     (rt',v@instr)
                  end
            ) (I.TEMP (Temp.newtemp (tr_ty te t)),[]) v
       in
          (te,ve,se,rt,v')
       end
     | tr_exp te ve se (A.ECON (i,NONE,t,p)) =
       let
          (* Layout for constructors is: { int index, i8* data } *)
          val conty = Temp.aggr [Temp.i32, Temp.ptr Temp.i8]
          val tt = I.TEMP (Temp.newtemp conty)
          val tt' = I.TEMP (Temp.newtemp conty)
          val rt' = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val rt = I.TEMP (Temp.newtemp (Temp.ptr conty))
          val instr = [I.MOVE (tt,I.TUPLE_INS (I.UNDEF conty,I.CONST i,0)),
                       I.MOVE (tt',I.TUPLE_INS (tt,I.NULL (Temp.ptr Temp.i8),1)),
                       I.MOVE (rt',I.MALLOC (conty,1)),
                       I.MOVE (rt,I.BITCAST (rt',Temp.ptr (conty))),
                       I.STORE (tt',rt)]
       in
          (te,ve,se,rt,instr)
       end
     | tr_exp te ve se (A.ECON (i,SOME (e,t1),t2,p)) =
       let
          val (_,ve',_,e',einstr) = tr_exp te ve se e

          (* Layout for constructors is: { int index, i8* data } *)
          val conty = Temp.aggr [Temp.i32, Temp.ptr Temp.i8]
          val tt = I.TEMP (Temp.newtemp conty)
          val tt' = I.TEMP (Temp.newtemp conty)

          val tmp1 = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val tmp2' = I.TEMP (Temp.newtemp (Temp.ptr (Temp.i8)))
          val tmp2 = I.TEMP (Temp.newtemp (Temp.ptr (tr_ty te t1)))

          val rt' = I.TEMP (Temp.newtemp (Temp.ptr Temp.i8))
          val rt = I.TEMP (Temp.newtemp (Temp.ptr conty))
          
          val instr = [I.MOVE (tmp2',I.MALLOC (tr_ty te t1,1)),
                       I.MOVE (tmp2, I.BITCAST (tmp2',Temp.ptr (tr_ty te t1))),
                       I.STORE (e',tmp2),
                       I.MOVE (tmp1,I.BITCAST (tmp2,Temp.ptr Temp.i8)),
                       I.MOVE (tt,I.TUPLE_INS (I.UNDEF conty,I.CONST i,0)),
                       I.MOVE (tt',I.TUPLE_INS (tt,tmp1,1)),
                       I.MOVE (rt',I.MALLOC (conty,1)),
                       I.MOVE (rt,I.BITCAST (rt',Temp.ptr conty)),
                       I.STORE (tt',rt)]
       in
          (te,ve,se,rt,einstr @ instr)
       end
     | tr_exp te ve se (A.ELET _) = raise Fail "tr_exp: ELET remains!" 
     | tr_exp te ve se (A.EANN (e,t,p)) = tr_exp te ve se e
     | tr_exp te ve se (A.ERAISE (e,p)) =
          (te,ve,se,I.UNDEF Temp.void,[])
     | tr_exp te ve se k = raise Fail ("tr_exp: Not implemented")
                 
   and tr_ast te ve se (A.DVAL (s,e,t,p)) =
       let
          val _ = Debug.print Debug.debug ("tr_ast: DVAL\n")
          val (_,_,_,e',in1) = tr_exp te ve se e
          
          val rt = if is_simple e' then e' else I.TEMP (Temp.newtemp (tr_ty te t))
          val ve' = S.enter (ve,s,rt)
          val mv = if is_simple e' then [] else [I.MOVE (rt,e')]
       in
          (te,ve',se,in1 @ mv)
       end
     | tr_ast te ve se (A.DVALREC (A.DVAL (s,e,t1,p1),t2,p2)) =
       let
          val rt = Temp.newglobal (tr_ty te t1)
          val ve' = S.enter (ve,s,I.NAME rt)
          val (_,_,_,e',in1) = tr_exp te ve' se e
          val _ = add_fragment (FALIAS (I.NAME rt, e'))
       in
          (te,ve',se,in1)
       end
     | tr_ast te ve se (A.DVALREC (a,t,p)) =
         tr_ast te ve se a
     | tr_ast te ve se (A.DMUTUAL (v,p)) = 
         Vector.foldl (fn (e,(te,ve,se,b)) =>
            let
               val (te',ve',se',b') = tr_ast te ve se e
            in
               (te',ve',se',b@b')
            end) (te,ve,se,[]) v 
     | tr_ast te ve se (A.DSTRUCTURE (s,c,v,p)) = 
       let
          val (te',ve',se',instr) =
            Vector.foldl (fn (e,(te,ve,se,b)) =>
            let
               val (te',ve',se',b') = tr_ast te ve se e
            in
               (te',ve',se',b@b')
            end) (te,ve,se,[]) v 
          val se'' = S.enter (se,s,StructEntry (te',ve',se'))
       in
          (te,ve,se'',instr)
       end
     | tr_ast te ve se (A.DSTRUCTASSIGN (s,e,p)) =
       let
          fun res se [] = raise Fail "tr_ast te encountered empty long id"
            | res se [h] = (case (S.look (se,h)) of
                               SOME e => e
                             | NONE => raise Fail "tr_ast te unknown struct id")
            | res se (h::t) = (case (S.look (se,h)) of
                                 SOME (StructEntry (_,_,e)) => res e t
                               | NONE => raise Fail "tr_ast: struct id")
   
          val rhs = (case e of 
                        (A.EID (rhs,_,p)) =>
                           (case (S.look (se,rhs)) of 
                              NONE => raise Fail "tr_ast: DSTRUCTASSIGN"
                            | SOME e' => e')
                      | (A.ELID (l,t,p)) => res se l)
          val se' = S.enter (se,s,rhs)
       in
          (te,ve,se',[])
       end
     | tr_ast te ve se (A.DOPEN (e,p)) =
       let
          fun sb se [] = Error.fail ("Empty long-id in 'open'",p)
               | sb se [h] = (case (S.look (se,h)) of 
                                 (SOME e) => e
                               | NONE => Error.fail("Invalid id in 'open'",p))
               | sb se (h::t) = (case (S.look (se,h)) of
                                  (SOME (StructEntry (_,_,e))) => sb e t
                                | NONE => Error.fail("Invalid id in 'open'",p))

             val a'' = case e of A.EID (n,t,p') => sb se [n]
                                | A.ELID (nl,t,p') => sb se nl

             val (ste,sve,sse) = 
               case a'' of
                  StructEntry s => s
                | _ => raise 
                  Fail "translate.k: Attempted to open non-struct!"

             val tkeys = 
                     map (fn (k,v) => (S.unhash k, v)) (S.keys ste)
             val vkeys = 
                     map (fn (k,v) => (S.unhash k, v)) (S.keys sve)
             val skeys = 
                     map (fn (k,v) => (S.unhash k, v)) (S.keys sse)

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
          (te',ve',se',[])
       end
     | tr_ast te ve se (A.DIMPORT (i1,t,i2,p)) =
       let
          val bi = I.BUILTIN (S.name i1)
          val ve' = S.enter (ve,i2,bi)
       in
          (te,ve',se,[])
       end
     | tr_ast te ve se (A.DNULL p) = (te,ve,se,[])
     | tr_ast te ve se _ = raise Fail "tr_ast: Not implemented"

   val tr_venv_base = Builtins.builtins

   fun translate te ast =
      let
         val (te,ve,se,ast') =
            List.foldl (fn (a,(te,ve,se,b)) =>
                    let
                       val (te',ve',se',a') = tr_ast te ve se a
                    in
                       (te',ve',se',b@a')
                    end) (te,tr_venv_base,S.empty,[]) ast
         val fr = !fragments
         val _ = fragments := []
      in
         (te,ve,se,ast',fr)
      end
end
