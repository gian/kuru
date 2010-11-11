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
*    Remove all infix operator application and replace it with
*    tupled function application.  e.g.:
*  1 + 3 --> +(1,3)
******************************************************************************)

structure AstInfixApply =
struct
   open ParseTree

   fun apply tenv venv pt = 
   let
      fun s (AndAlso (a,b,pos)) = 
            App(Ident("andalso",pos),
                TupleExp ([s a, s b],pos),pos)
        | s (OrElse (a,b,pos)) = 
            App(Ident("orelse",pos),
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
        | s t = t
   in
      (tenv,venv,map s pt) 
   end

end
