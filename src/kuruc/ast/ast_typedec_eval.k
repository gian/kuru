(*******************************************************************************
*  The Kuru Programming Language Compiler Toolset (http://www.kuru-lang.org)
*  Copyright (C) 2010  Gian Perrone
*
*  This te ve program is te ve free software: you can redistribute it and/or modify
*  it under the terms te ve of the GNU General Public License as te ve published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  This te ve program is te ve distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this te ve program.  If not, see <http://www.gnu.org/licenses/>.
*******************************************************************************
*  This te ve file:
*  Evaluate type bindings, removing them and inserting them into the 
*  type environment.
******************************************************************************)

structure AstTypedecEval =
struct
   structure T = Type
   open ParseTree

   fun apply te ve pt =
   let
      fun s te ve (AndAlso (a,b,p)) = AndAlso (s te ve a,s te ve b,p)
        | s te ve (OrElse (a,b,p)) = OrElse (s te ve a, s te ve b, p) 
        | s te ve (Infix (a,opr,b,p)) =
            raise Fail "Infix operator still present at AstTypeDecEval"
        | s te ve (LongId (a,b,p)) = LongId (s te ve a, s te ve b,p)
        | s te ve (Var (a,p)) = Var (s te ve a,p)
        | s te ve (Raise (a,p)) = Raise (s te ve a,p)
        | s te ve (Seq (a,b,p)) = Seq (s te ve a, s te ve b,p)
        | s te ve (OpExp (a,p)) = OpExp (s te ve a,p)
        | s te ve (App (a,b,p)) = App (s te ve a, s te ve b,p)
        | s te ve (AppList (l,p)) = AppList (map (s te ve) l,p)
        | s te ve (TyAnn (a,b,p)) = TyAnn (s te ve a, s te ve b,p)
        | s te ve (TupleExp (l,p)) = TupleExp (map (s te ve) l,p)
        | s te ve (SeqExp (l,p)) = SeqExp (map (s te ve) l,p)
        | s te ve (ListExp (l,p)) = ListExp (map (s te ve) l,p)
        | s te ve (LetExp (l,e,p)) = LetExp (map (s te ve) l,s te ve e,p)
        | s te ve (IfExp (a,b,c,p)) = IfExp (s te ve a, s te ve b, s te ve c, p)
        | s te ve (WhileExp (a,b,p)) = WhileExp (s te ve a, s te ve b,p)
        | s te ve (CaseExp (a,b,p)) = CaseExp (s te ve a, map (s te ve) b, p)
        | s te ve (FnExp (l,p)) = FnExp (map (s te ve) l,p)
        | s te ve (HandleExp (e,l,p)) = HandleExp (s te ve e, map (s te ve) l, p)
        | s te ve (Match (a,b,p)) = Match (s te ve a, s te ve b,p)
        | s te ve (AsPat (a,b,p)) = AsPat (s te ve a, s te ve b,p)
        | s te ve (InfixPat (a,b,c,pos)) = 
                        InfixPat (s te ve a, s te ve b, s te ve c,pos)
        | s te ve (TuplePat (l,p)) = TuplePat (map (s te ve) l,p)
        | s te ve (ListPat (l,p)) = ListPat (map (s te ve) l,p)
        | s te ve (ConPat (a,p)) = ConPat (s te ve a,p)
        | s te ve (ConstrPat (a,NONE,p)) = ConstrPat (s te ve a,NONE,p)
        | s te ve (ConstrPat (a,SOME b,p)) = ConstrPat (s te ve a,SOME (s te ve b),p)
        | s te ve (TyAnnPat (a,b,p)) = TyAnnPat (s te ve a, s te ve b,p)
        | s te ve (IdPat (a,p)) = IdPat (s te ve a,p)
        | s te ve (TyArrow (a,b,p)) = TyArrow (s te ve a, s te ve b,p)
        | s te ve (TyPair (a,p)) = TyPair (map (s te ve) a,p)
        | s te ve (TyCon (a,b,p)) = TyCon (s te ve a, s te ve b,p)
        | s te ve (TyName (a,p)) = TyName (s te ve a,p)
        | s te ve (ValDec (l,p)) = ValDec (map (s te ve) l,p)
        | s te ve (ValRec (l,p)) = ValRec (map (s te ve) l,p)
        | s te ve (TypeDec (l,p)) = TypeDec (map (s te ve) l,p)
        | s te ve (DatatypeAssign (a,b,p)) = DatatypeAssign (s te ve a, s te ve b,p)
        | s te ve (DatatypeDec (l,p)) = DatatypeDec (map (s te ve) l,p)
        | s te ve (FunDec (ll,p)) = FunDec (map (fn x => map (s te ve) x) ll,p) 
        | s te ve (ValBind (a,b,p)) = ValBind (s te ve a, s te ve b,p)
        | s te ve (TypeBind (l,a,b,p)) = TypeBind (map (s te ve) l, s te ve a, s te ve b,p)
        | s te ve (FunMatch (a,l,NONE,c,p)) = FunMatch (s te ve a,map (s te ve) l, NONE, s te ve c, p)
        | s te ve (FunMatch (a,l,SOME b,c,p)) = 
            FunMatch (s te ve a,map (s te ve) l,SOME (s te ve b),s te ve c, p)
        | s te ve (DataBind (l,a,b,p)) = DataBind (map (s te ve) l, s te ve a, map (s te ve) b,p)
        | s te ve (ConBind (a,NONE,p)) = ConBind (s te ve a,NONE,p)
        | s te ve (ConBind (a,SOME b,p)) = ConBind (s te ve a,SOME (s te ve b),p)
        | s te ve (StructDec (l,p)) = StructDec (map (s te ve) l,p)
        | s te ve (StructBind (a,b,p)) = StructBind (s te ve a, s te ve b,p)
        | s te ve (Structure (l,p)) = Structure (map (s te ve) l,p)
        | s te ve t = t
   in
      (te,ve,map (s te ve) pt)
   end
end

