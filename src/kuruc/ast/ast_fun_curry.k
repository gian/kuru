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
*  Replace all curried functions with multiple fn instances, e.g.:
*  fun f x y = E --> val f = fn x => fn y => E
* 
*  fun i a b c 0 = a + b + c * 10
*  | i a b c d = a - b - c - d
*    -->
*  val rec i = fn x1 => fn x2 => fn x3 => fn x4 =>
* 		(case (x1,x2,x3,x4) of (a,b,c,0) => a + b + c * 10
* 		                     | (a,b,c,d) => a - b - c - d)
*
*  Preconditions:
*
*  Postcondition:
*    There are no "FunDec" statements anywhere in the parse tree.
******************************************************************************)

structure AstFunCurry =
struct
   open ParseTree

   val fr = ref 0 (* To generate fresh names *)
   fun fresh () = (fr := !fr + 1; "_p" ^ Int.toString (!fr))

   fun apply tenv venv pt =
   let
      fun tuplefm (FunMatch (_,p,_,e,pos)) = Match (TuplePat (p,pos), e, pos)
        | tuplefm _ = raise Fail "AstFunCurry.tuplefm called with non-FunMatch" 

      fun casefml [] = raise Fail "AstFunCurry.casefml matches = []"
        | casefml [FunMatch (n,[IdPat (Ident (sy,p),p')],_,e,pos)] = 
        let
           
        in
           ValRec ([ValBind (IdPat (n,pos), 
              FnExp ([Match(IdPat (Ident (sy,p),pos),e,pos)],pos),pos)],pos)
        end
        | casefml l = 
         let
            val (FunMatch (n,pat,_,_,pos)) = 
               case hd l of FunMatch c => FunMatch c
                          | _ => raise Fail "Non-funmatch"
            
            fun mkfn p e pos = 
                  FnExp ([Match (IdPat ((Ident (p,pos),pos)),e,pos)],pos)

            val newnames = map (fn _ => fresh()) pat

            fun mkfns ce = List.foldr (fn (v,e) => mkfn v e pos) ce newnames

            val tupled = TupleExp (map (fn x => Ident (x,pos)) newnames, pos)

            val cases = map tuplefm l
         in
            ValRec ([ValBind (IdPat (n,pos), mkfns (CaseExp (tupled,cases,pos)),pos)],pos)
         end

      fun fnand ([],_) = raise Fail "AstFunCurry.fnand matches = []"
        | fnand (ll,pos) = s (ValDec (map casefml ll, pos))

      and s (AndAlso (a,b,p)) = AndAlso (s a,s b,p)
        | s (OrElse (a,b,p)) = OrElse (s a, s b, p) 
        | s (Infix (a,opr,b,p)) = Infix (s a, s opr, s b, p)
        | s (LongId (a,b,p)) = LongId (s a, s b,p)
        | s (Var (a,p)) = Var (s a,p)
        | s (Raise (a,p)) = Raise (s a,p)
        | s (Seq (a,b,p)) = Seq (s a, s b,p)
        | s (OpExp (a,p)) = OpExp (s a,p)
        | s (App (a,b,p)) = App (s a, s b,p)
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
        | s (TyPair (a,b,p)) = TyPair (s a, s b,p)
        | s (TyCon (a,b,p)) = TyCon (s a, s b,p)
        | s (TyName (a,p)) = TyName (s a,p)
        | s (ValDec (l,p)) = ValDec (map s l,p)
        | s (ValRec (l,p)) = ValRec (map s l,p)
        | s (TypeDec (l,p)) = TypeDec (map s l,p)
        | s (DatatypeAssign (a,b,p)) = DatatypeAssign (s a, s b,p)
        | s (DatatypeDec (l,p)) = DatatypeDec (map s l,p)
        | s (FunDec (ll,p)) = fnand (ll,p) 
        | s (ValBind (a,b,p)) = ValBind (s a, s b,p)
        | s (TypeBind (l,a,b,p)) = TypeBind (map s l, s a, s b,p)
        | s (FunMatch (a,l,NONE,c,p)) = FunMatch (s a,map s l, NONE, s c, p)
        | s (FunMatch (a,l,SOME b,c,p)) = 
            FunMatch (s a,map s l,SOME (s b),s c, p)
        | s (DataBind (l,a,b,p)) = DataBind (map s l, s a, map s b,p)
        | s (ConBind (a,NONE,p)) = ConBind (s a,NONE,p)
        | s (ConBind (a,SOME b,p)) = ConBind (s a,SOME (s b),p)
        | s (StructDec (l,p)) = StructDec (map s l,p)
        | s (StructBind (a,b,p)) = StructBind (s a, s b,p)
        | s (Structure (l,p)) = Structure (map s l,p)
        | s t = t
   in
      (tenv,venv,map s pt)
   end
end

