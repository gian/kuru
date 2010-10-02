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
*    Abstract Parse tree definition and pretty printer. 
******************************************************************************)

structure ParseTree =
struct

type pos = int

   datatype pt =
      Int of bool * string * pos
    | Real of bool * string * string * (bool * string) option * pos
    | Char of string * pos
    | String of string * pos
    | Ident of string * pos
    | LongId of pt * pt * pos
    | Var of pt * pos
    | Raise of pt * pos
    | AndAlso of pt * pt * pos
    | OrElse of pt * pt * pos
    | Seq of pt * pt * pos
    | Infix of pt * pt * pt * pos
    | OpExp of pt * pos
    | App of pt * pt * pos
    | TyAnn of pt * pt * pos
    | TupleExp of pt list * pos
    | UnitExp of pos
    | SeqExp of pt list * pos
    | ListExp of pt list * pos
    | LetExp of pt list * pt * pos
    | IfExp of pt * pt * pt * pos
    | WhileExp of pt * pt * pos
    | CaseExp of pt * pt list * pos
    | FnExp of pt list * pos
    | HandleExp of pt * pt list * pos
    | Match of pt * pt * pos
    | AsPat of pt * pt * pos
    | UnitPat of pos
    | InfixPat of pt * pt * pt * pos
    | WildcardPat of pos
    | TuplePat of pt list * pos
    | ListPat of pt list * pos
    | ConPat of pt * pos
    | ConstrPat of pt * pt option * pos
    | TyAnnPat of pt * pt * pos
    | IdPat of pt * pos
    | TyArrow of pt * pt * pos
    | TyPair of pt * pt * pos
    | TyCon of pt * pt * pos
    | TyName of pt * pos
    | ValDec of pt list * pos
    | TypeDec of pt list * pos
    | DatatypeAssign of pt * pt * pos
    | DatatypeDec of pt list * pos
    | FunDec of pt list list * pos
    | ValBind of pt * pt * pos
    | TypeBind of pt list * pt * pt * pos
    | FunMatch of pt * pt list * pt option * pt * pos
    | DataBind of pt list * pt * pt list * pos
    | ConBind of pt * pt option * pos
    | StructDec of pt list * pos
    | StructBind of pt * pt * pos
    | Structure of pt list * pos

   fun pp (Int (b,s,_)) = "Int [" ^ (if b then "~" else "") ^ s ^ "]" 
     | pp (Real (b,s1,s2,NONE,_)) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^ "]" 
     | pp (Real (b,s1,s2,SOME (b2,s3),_)) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^
         "e" ^ (if b2 then "~" else "") ^ s3 ^ "]"
     | pp (Char (s,_)) = "Char [#\"" ^ s ^ "\"]"
     | pp (String (s,_)) = "String [\"" ^ s ^ "\"]"
     | pp (Ident (s,_)) = "Ident [" ^ s ^ "]"
     | pp (Var (p,_)) = "Var [" ^ pp p ^ "]"
     | pp (Raise (p,_)) = "Raise [" ^ pp p ^ "]"
     | pp (LongId (l1,l2,_)) = "LongId [" ^ pp l1 ^ "." ^ pp l2 ^ "]"
     | pp (AndAlso (p1,p2,_)) = "AndAlso [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (OrElse (p1,p2,_)) = "OrElse [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (Seq (p1, p2,_)) = pp p1 ^ "\n" ^ pp p2
     | pp (Infix (a,b,c,_)) = "Infix [" ^ pp a ^ " " ^ pp b ^ " " ^ pp c ^ "]"
     | pp (OpExp (p,_)) = "Op [" ^ pp p ^ "]"
     | pp (App (p1,p2,_)) = "App [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (TyAnn (p1,p2,_)) = "TyAnn [" ^ pp p1 ^ " : " ^ pp p2 ^ "]"
     | pp (TupleExp (l,p)) = "Tuple [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (UnitExp _) = "Unit []"
     | pp (SeqExp (l,_)) = "Seq [" ^ (String.concatWith "; " (map pp l)) ^ "]"
     | pp (ListExp (l,_)) = "List [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (LetExp (d,e,_)) = "Let [" ^ (String.concatWith "; " (map pp d)) ^ 
                            " in " ^ pp e ^ " end]"
     | pp (IfExp (e1,e2,e3,_)) = "If [" ^ pp e1 ^ " then " ^ pp e2 ^ " else " ^
                                  pp e3 ^ "]"
     | pp (WhileExp (e1,e2,_)) = "While [" ^ pp e1 ^ " do " ^ pp e2 ^ "]"
     | pp (CaseExp (e1,m,_)) = "Case [" ^ pp e1 ^ " of " ^ 
                                 (String.concatWith " | " (map pp m)) ^ "]"
     | pp (FnExp (m,_)) = "Fn [" ^(String.concatWith " | " (map pp m)) ^ "]"
     | pp (HandleExp (e,m,_)) = "Handle [" ^ pp e ^ ", " ^
                                 (String.concatWith " | " (map pp m)) ^ "]"
     | pp (AsPat (p1,p2,_)) = "AsPat [" ^ pp p1 ^ " as " ^ pp p2 ^ "]"
     | pp (InfixPat (p1,p2,p3,_)) = "InfixPat [" ^ pp p1 ^ " " ^ pp p2 ^ 
                                     " " ^ pp p3 ^ "]"
     | pp (WildcardPat _) = "WildcardPat []"
     | pp (TuplePat (l,_)) = "TuplePat [" ^ 
            (String.concatWith ", " (map pp l)) ^ "]"
     | pp (UnitPat _) = "UnitPat []"
     | pp (ListPat (l,_)) = "ListPat [" ^ 
            (String.concatWith ", " (map pp l)) ^ "]"
     | pp (ConPat (c,_)) = "ConPat [" ^ pp c ^ "]"
     | pp (ConstrPat (i,NONE,_)) = "ConstrPat [" ^ pp i ^ "]"
     | pp (ConstrPat (i,SOME p,_)) = "ConstrPat ["^ pp i ^ " "^ pp p ^"]"
     | pp (TyAnnPat (i,p,_)) = "TyAnnPat ["^ pp i ^ " : "^ pp p ^"]"
     | pp (IdPat (c,_)) = "IdPat [" ^ pp c ^ "]"
     | pp (TyArrow (t1,t2,_)) = pp t1 ^ " -> " ^ pp t2
     | pp (TyPair (t1,t2,_)) = pp t1 ^ " * " ^ pp t2
     | pp (TyCon (t1,t2,_)) = pp t1 ^ " " ^ pp t2
     | pp (TyName (t1,_)) = pp t1
     | pp (Match (p,e,_)) = "Match [" ^ pp p ^ " => " ^ pp e ^ "]"
     | pp (ValDec (b,_)) = 
         "ValDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]"
     | pp (TypeDec (b,_)) =
         "TypeDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]"
     | pp (DatatypeAssign (i1,i2,_)) = 
                     "DatatypeAssign [" ^ pp i1 ^ " = " ^ pp i2 ^ "]"
     | pp (DatatypeDec (d,_)) =
         "DatatypeDec [" ^ (String.concatWith "\nand " (map pp d)) ^ "]"
     | pp (FunDec (d,_)) = 
         "FunDec [" ^ (String.concatWith "\nand " 
            (map (fn l => String.concatWith "\n  | " (map pp l)) d)) ^ "]"
     | pp (ValBind (p,e,_)) = "ValBind [" ^ pp p ^ " = " ^ pp e ^ "]"
     | pp (TypeBind (v,p,e,_)) = "TypeBind [" ^ (String.concatWith "," 
                                 (map pp v))
                                ^ " " ^ pp p ^ " = " ^ pp e ^ "]"
     | pp (FunMatch (i,l,NONE,e,_)) = "FunMatch [" ^ pp i ^ " " ^ 
                                    (String.concatWith " " (map pp l)) ^ 
                                    " = " ^ pp e ^ "]"
     | pp (FunMatch (i,l,SOME t,e,_)) = "FunMatch [" ^ pp i ^ " " ^ 
                                    (String.concatWith " " (map pp l)) ^ 
                                    " : " ^ pp t ^
                                    " = " ^ pp e ^ "]"
     | pp (DataBind (tv,i,b,_)) = "DataBind [(" ^ (String.concatWith "," (map pp tv)) ^
                                 ") " ^ pp i ^ " =\n   " ^ 
                                 (String.concatWith "\n | " (map pp b)) ^ "]"
     | pp (ConBind (i,NONE,_)) = "ConBind [" ^ pp i ^ "]"
     | pp (ConBind (i,SOME t,_)) = "ConBind [" ^ pp i ^ " of " ^ pp t ^ "]"
     | pp (StructDec (l,_)) = "Structure [" ^
                  (String.concatWith "\nand " (map pp l)) ^ "]"
     | pp (StructBind (i,b,_)) = "StructBind ["^ pp i ^" = "^ pp b ^ "]" 
     | pp (Structure (l,_)) = "Struct [\n   " ^
                  (String.concatWith "\n   " (map pp l)) ^ "]"

   fun prettyPrint p = pp p

end

