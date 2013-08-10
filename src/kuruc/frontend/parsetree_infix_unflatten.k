(******************************************************************************
* The Kuruc Compiler (http://www.kuru-lang.org)
* Copyright (c) 2010-2013, Gian Perrone
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*
* - Redistributions of source code must retain the above copyright notice, this
*   list of conditions and the following disclaimer.
*
* - Redistributions in binary form must reproduce the above copyright notice,
*   this list of conditions and the following disclaimer in the documentation
*   and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
* POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************
*  This file:
*    Process infix and infixr declarations and expand App nodes to Infix exps.
*  e.g.
*    infix foo
*    App (a, App ("foo", b)) --> Infix (a, "foo", b)
******************************************************************************)

structure ParseTreeInfixUnflatten =
struct
   open ParseTree

   datatype binddir = BLeft | BRight
   
   val infixes = ref [] : (int * pt * binddir) list ref


   fun ins ([],p,opr,d) = [(p,opr,d)]
     | ins ((p1,o1,d1)::t,p2,o2,d2) = 
          if p1 >= p2 
             then (p2,o2,d2) :: (List.filter (fn (_,x,_) => x <> o2) 
                                    ((p1,o1,d1) :: t))
             else (p1,o1,d1) :: ins (t,p2,o2,d1)

   fun add_infix ("",opr,d) = infixes := ins (!infixes,8,opr,d)
     | add_infix (p,opr,d) = (infixes :=
            ins(!infixes,valOf(Int.fromString p),opr,d))

   fun is_operator (Ident (i,_)) = 
         List.exists (fn (_,Ident (x,_),_) => x = i
                       | _ => false) (!infixes)
     | is_operator (IdPat (i, _)) = is_operator i
     | is_operator _ = false

   fun apply tenv venv pt = 
   let
      fun s i (AndAlso (a,b,pos)) = AndAlso (s i a, s i b, pos) 
        | s i (OrElse (a,b,pos)) = OrElse (s i a, s i b, pos)
        | s i (Infix (a,opr,b,pos)) = Infix (s i a, opr, s i b, pos)
        | s i (LongId (a,b,p)) = LongId (s i a, s i b,p)
        | s i (Var (a,p)) = Var (s i a,p)
        | s i (Raise (a,p)) = Raise (s i a,p)
        | s i (Seq (a,b,p)) = Seq (s i a, s i b,p)
        | s i (OpExp (a,p)) = OpExp (s i a,p)
        | s i (AppList ([],p)) = AppList ([],p)
        | s i (AppList ([a],p)) = AppList ([s i a],p)
        | s i (AppList ([a,b],p)) = AppList ([s i a, s i b],p)
        | s i (AppList (l,p)) =
          let
             val (opr,d) = i

             fun fnapp (h1::h2::t) = 
                 if not (is_operator h1) andalso not (is_operator h2) then
                    fnapp ((AppList ([h1,h2],p)) :: t)
                 else h1 :: h2 :: fnapp t
               | fnapp [h] = [h]
               | fnapp [] = []


             (* FIXME: This is always infixl!  Need to implement infixr *)

             (* 1 + 2 + 3 + 4 --> 
                (((1 + 2) + 3) + 4) *)
             fun infixifyl [] = []
               | infixifyl [h] = [h]
               | infixifyl (h1::h2::h3::t) =
                  (case h2 of (Ident (h2',p')) =>
                     (if h2' = opr then infixifyl ([Infix (s i h1,
                                                    h2,
                                                    s i h3,
                                                    p)] @ t)
                                   else ((s i h1) :: (infixifyl (h2 :: h3 :: t))))
                            | _ => (s i h1) :: (infixifyl (h2 :: h3 :: t)))
               | infixifyl (h::t) = s i h :: infixifyl t

             fun infixifyr [] = []
               | infixifyr [h] = [h]
               | infixifyr (h1::h2::t) =
                  (case h2 of (Ident (h2',p')) =>
                     (if h2' = opr then [Infix (s i h1,
                                              h2,
                                              AppList (infixifyr t,p),
                                              p')]
                                  else ((s i h1) :: (infixifyr (h2::t))))
                            | _ => (s i h1) :: (infixifyr (h2 :: t)))
          in
             case d of BLeft =>
                        AppList (infixifyl (fnapp l), p)
                    | BRight =>
                        AppList (infixifyr (fnapp l), p) 
          end
        | s i (App (a,b,p)) = App (s i a, s i b,p)
        | s i (TyAnn (a,b,p)) = TyAnn (s i a, s i b,p)
        | s i (TupleExp (l,p)) = TupleExp (map (s i) l,p)
        | s i (SeqExp (l,p)) = SeqExp (map (s i) l,p)
        | s i (ListExp (l,p)) = ListExp (map (s i) l,p)
        | s i (LetExp (l,e,p)) = LetExp (map (s i) l,s i e,p)
        | s i (IfExp (a,b,c,p)) = IfExp (s i a, s i b, s i c, p)
        | s i (WhileExp (a,b,p)) = WhileExp (s i a, s i b,p)
        | s i (CaseExp (a,b,p)) = CaseExp (s i a, map (s i) b, p)
        | s i (FnExp (l,p)) = FnExp (map (s i) l,p)
        | s i (HandleExp (e,l,p)) = HandleExp (s i e, map (s i) l, p)
        | s i (Match (a,b,p)) = (Debug.print Debug.debugmore "Unflatten Match\n";
                                 Match (s i a, s i b,p))
        | s i (AsPat (a,b,p)) = AsPat (s i a, s i b,p)
        | s i (InfixPat (a,b,c,pos)) = InfixPat (s i a, s i b, s i c,pos)
        | s i (TuplePat (l,p)) = TuplePat (map (s i) l,p)
        | s i (ListPat (l,p)) = ListPat (map (s i) l,p)
        | s i (ConPat (a,p)) = ConPat (s i a,p)
        | s i (ConstrPat (a,NONE,p)) = ConstrPat (s i a,NONE,p)
        | s i (ConstrPat (a,SOME b,p)) = ConstrPat (s i a,SOME (s i b),p)
        | s i (TyAnnPat (a,b,p)) = TyAnnPat (s i a, s i b,p)
        | s i (IdPat (a,p)) = IdPat (s i a,p)
        | s i (TyArrow (a,b,p)) = TyArrow (s i a, s i b,p)
        | s i (TyPair (a,p)) = TyPair (map (s i) a,p)
        | s i (TyCon (a,b,p)) = TyCon (s i a, s i b,p)
        | s i (TyName (a,p)) = TyName (s i a,p)
        | s i (ValDec (l,p)) = ValDec (map (s i) l,p)
        | s i (InfixrDec (prec,opr,p)) = (add_infix (prec,opr,BRight);
                                        InfixrDec (prec,opr,p))
        | s i (InfixDec (prec,opr,p)) = (add_infix (prec,opr,BLeft);
                                        InfixDec (prec,opr,p))
        | s i (ValRec (l,p)) = ValRec (map (s i) l,p)
        | s i (TypeDec (l,p)) = TypeDec (map (s i) l,p)
        | s i (DatatypeAssign (a,b,p)) = DatatypeAssign (s i a, s i b,p)
        | s i (DatatypeDec (l,p)) = DatatypeDec (map (s i) l,p)
        | s i (FunDec (ll,p)) = FunDec (map (fn l => map (s i) l) ll,p)
        | s i (ValBind (a,b,p)) = ValBind (s i a, s i b,p)
        | s i (TypeBind (l,a,b,p)) = TypeBind (map (s i) l, s i a, s i b,p)
        | s i (FunMatch (a, [IdPat (b,p'), c], t, d, p)) =
            (* Defs like: fun a + b = ... *)
            if is_operator b then
               FunMatch (s i b, [(s i) (TuplePat ([IdPat (a, p), c], p))], t, s i d, p)
            else FunMatch (s i a, map (s i) [IdPat (b,p'), c], t, s i d, p)
        | s i (FunMatch (a,l,NONE,c,p)) = FunMatch (s i a,map (s i) l, NONE, s i c, p)
        | s i (FunMatch (a,l,SOME b,c,p)) = 
            FunMatch (s i a,map (s i) l,SOME (s i b),s i c, p)
        | s i (DataBind (l,a,b,p)) = DataBind (map (s i) l, s i a, map (s i) b,p)
        | s i (ConBind (a,NONE,p)) = ConBind (s i a,NONE,p)
        | s i (ConBind (a,SOME b,p)) = ConBind (s i a,SOME (s i b),p)
        | s i (StructDec (l,p)) = StructDec (map (s i) l,p)
        | s i (StructBind (a,c,b,p)) = StructBind (s i a,c, s i b,p)
        | s i (Structure (l,p)) = Structure (map (s i) l,p)
        | s i t = t 

      fun conv l =
         let
            val _ = Debug.print Debug.debugmore
                        ("Sz: " ^ Int.toString (length (!infixes)) ^ "\n")
            val _ = s ("",BLeft) l
         in
            List.foldl (fn ((pr,Ident (opr,_),d),l) =>
                             (Debug.print Debug.debugmore ("Opr:" ^ opr ^ "\n");
                                 s (opr,d) l)
                            | _ => raise Fail "Invalid non-Ident operator")
                            l (List.rev (!infixes))
         end

   in
      (tenv,venv,map conv pt) 
   end

end

