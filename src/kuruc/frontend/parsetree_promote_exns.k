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
*    Turn exceptions into a top-level (global) datatype declaration of
*    type exn.
******************************************************************************)

structure ParseTreePromoteExns =
struct
   open ParseTree

   val exns = ref [] : pt list ref

   fun add_con x = exns := !exns @ [ConBind x]

   fun apply tenv venv pt = 
   let
      fun s (AndAlso (a,b,pos)) = 
            App(Ident("_andalso",pos),
                TupleExp ([s a, s b],pos),pos)
        | s (OrElse (a,b,pos)) = 
            App(Ident("_orelse",pos),
               TupleExp ([s a, s b],pos),pos)
        | s (Infix (a,opr,b,pos)) =
            App(opr,
               TupleExp ([s a, s b],pos),pos)
        | s (LongId (a,b,p)) = LongId (s a, s b,p)
        | s (Var (a,p)) = Var (s a,p)
        | s (Raise (a,p)) = Raise (s a,p)
        | s (Seq (a,b,p)) = Seq (s a, s b,p)
        | s (OpExp (a,p)) = OpExp (s a,p)
        | s (App (a,b,p)) = App (s a, s b,p)
        | s (AppList (l,p)) = AppList (map s l,p)
        | s (TyAnn (a,b,p)) = TyAnn (s a, s b,p)
        | s (TupleExp (l,p)) = TupleExp (map s l,p)
        | s (SeqExp (l,p)) = SeqExp (map s l,p)
        | s (ListExp (l,p)) = ListExp (map s l,p)
        | s (LetExp (l,e,p)) = LetExp (map s l,s e,p)
        | s (IfExp (a,b,c,p)) = IfExp (s a, s b, s c, p)
        | s (WhileExp (a,b,p)) = WhileExp (s a, s b,p)
        | s (CaseExp (a,b,p)) = CaseExp (s a, map s b, p)
        | s (FnExp (l,p)) = FnExp (map s l,p)
        | s (HandleExp (e,l,p)) = HandleExp (s e, map s l, p)
        | s (Match (a,b,p)) = Match (s a, s b,p)
        | s (AsPat (a,b,p)) = AsPat (s a, s b,p)
        | s (InfixPat (a,b,c,pos)) = InfixPat (s a, s b, s c,pos)
        | s (TuplePat (l,p)) = TuplePat (map s l,p)
        | s (ListPat (l,p)) = ListPat (map s l,p)
        | s (ConPat (a,p)) = ConPat (s a,p)
        | s (ConstrPat (a,NONE,p)) = ConstrPat (s a,NONE,p)
        | s (ConstrPat (a,SOME b,p)) = ConstrPat (s a,SOME (s b),p)
        | s (TyAnnPat (a,b,p)) = TyAnnPat (s a, s b,p)
        | s (IdPat (a,p)) = IdPat (s a,p)
        | s (TyArrow (a,b,p)) = TyArrow (s a, s b,p)
        | s (TyPair (a,p)) = TyPair (map s a,p)
        | s (TyCon (a,b,p)) = TyCon (s a, s b,p)
        | s (TyName (a,p)) = TyName (s a,p)
        | s (ValDec (l,p)) = ValDec (map s l,p)
        | s (ValRec (l,p)) = ValRec (map s l,p)
        | s (TypeDec (l,p)) = TypeDec (map s l,p)
        | s (DatatypeAssign (a,b,p)) = DatatypeAssign (s a, s b,p)
        | s (DatatypeDec (l,p)) = DatatypeDec (map s l,p)
        | s (FunDec (ll,p)) = FunDec (map (fn l => map s l) ll,p)
        | s (ValBind (a,b,p)) = ValBind (s a, s b,p)
        | s (TypeBind (l,a,b,p)) = TypeBind (map s l, s a, s b,p)
        | s (FunMatch (a,l,NONE,c,p)) = FunMatch (s a,map s l, NONE, s c, p)
        | s (FunMatch (a,l,SOME b,c,p)) = 
            FunMatch (s a,map s l,SOME (s b),s c, p)
        | s (DataBind (l,a,b,p)) = DataBind (map s l, s a, map s b,p)
        | s (ConBind (a,NONE,p)) = ConBind (s a,NONE,p)
        | s (ConBind (a,SOME b,p)) = ConBind (s a,SOME (s b),p)
        | s (StructDec (l,p)) = StructDec (map s l,p)
        | s (StructBind (a,c, b,p)) = StructBind (s a, c, s b,p)
        | s (Structure (l,p)) = Structure (map s l,p)
        | s (ExnDec (a,b,p)) = (add_con (a,b,p);ExnDec (a,b,p))
        | s t = t

      val pt' = map s pt
      val exs =
         DatatypeDec ([DataBind ([],Ident ("exn",(0,"_INTERNAL_")),
                                                 !exns,
                                                 (0,"_INTERNAL_"))],
                                                 (0,"_INTERNAL_")) 

      (* TODO: This is probably the appropriate place to construct
         a global "handle" that preserves exception names *)
   in
      (tenv,venv,exs :: pt')
   end

end

