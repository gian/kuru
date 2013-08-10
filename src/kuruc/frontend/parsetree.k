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
* This file:
*    Concrete parse tree definition and pretty printer. 
* It is to this representation that various front-end transformations get
* applied, such as handling of infix operators (and their precedence).
* Eventually this gets marshalled into a proper TypedAST.t
******************************************************************************)

structure ParseTree =
struct
   type pos = int * string

   datatype ascription =
      AscribeOpaque of pt
    | Ascribe of pt
    | Implicit

   and pt =
      Int of bool * string * pos
    | Real of bool * string * string * (bool * string) option * pos
    | Char of int * pos
    | String of int list * pos
    | Ident of string * pos
    | LongId of pt * pt * pos
	 | TyId of pt * pos
    | Var of pt * pos
    | Raise of pt * pos
    | AndAlso of pt * pt * pos
    | OrElse of pt * pt * pos
    | Seq of pt * pt * pos
    | Infix of pt * pt * pt * pos
    | OpExp of pt * pos
    | App of pt * pt * pos
    | AppList of pt list * pos
    | TyAnn of pt * pt * pos
    | TupleExp of pt list * pos
    | UnitExp of pos
    | SeqExp of pt list * pos
    | ListExp of pt list * pos
    | NilExp of pos
    | LetExp of pt list * pt * pos
    | IfExp of pt * pt * pt * pos
    | WhileExp of pt * pt * pos
    | CaseExp of pt * pt list * pos
    | FnExp of pt list * pos
    | HandleExp of pt * pt list * pos
    | ExnDec of pt * pt option * pos
    | OpenDec of pt * pos
    | Match of pt * pt * pos
    | AsPat of pt * pt * pos
    | UnitPat of pos
    | InfixPat of pt * pt * pt * pos
    | WildcardPat of pos
    | TuplePat of pt list * pos
    | ListPat of pt list * pos
    | NilPat of pos
    | ImplicitPat of (pt * pt) list * pos
    | ConPat of pt * pos
    | ConstrPat of pt * pt option * pos
    | TyAnnPat of pt * pt * pos
    | IdPat of pt * pos
    | TyArrow of pt * pt * pos
    | TyPair of pt list * pos
    | TyCon of pt * pt * pos
    | TyName of pt * pos
    | TyClass of pt * pt * pt * pos
    | ValDec of pt list * pos
    | TypeDec of pt list * pos
    | InfixrDec of string * pt * pos 
    | InfixDec of string * pt * pos
    | DatatypeAssign of pt * pt * pos
    | DatatypeDec of pt list * pos
    | FunDec of pt list list * pos
    | ValBind of pt * pt * pos
	 | ValRec of pt list * pos
    | TypeBind of pt list * pt * pt * pos
    | FunMatch of pt * pt list * pt option * pt * pos
    | DataBind of pt list * pt * pt list * pos
    | ConBind of pt * pt option * pos
    | StructDec of pt list * pos
    | StructBind of pt * ascription * pt * pos
    | Structure of pt list * pos
    | ValDesc of pt list * pos
    | TypeDesc of pt list * pos
    | StructDesc of pt list * pos
    | ValDescBind of pt * pt * pos 
    | TypeDescBind of pt list * pt * pos 
    | StructDescBind of pt * pt * pos
    | SigBind of pt * pt * pos
    | Signature of pt list * pos
    | SigDec of pt list * pos
    | ClassDec of pt list * pos
    | ClassBind of pt * pt * pos
    | ClassImpl of pt * pt * pt * pos
    | Import of pt * pt * pt * pos

   fun istr [] = ""
     | istr (h::t) = String.str (Char.chr h) ^ istr t

   fun pp (Int (b,s,_)) = "Int [" ^ (if b then "~" else "") ^ s ^ "]" 
     | pp (Real (b,s1,s2,NONE,_)) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^ "]" 
     | pp (Real (b,s1,s2,SOME (b2,s3),_)) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^
         "e" ^ (if b2 then "~" else "") ^ s3 ^ "]"
     | pp (Char (s,_)) = "Char [#\"" ^ String.str (Char.chr s) ^ "\"]"
     | pp (String (s,_)) = "String [\"" ^ istr s ^ "\"]"
     | pp (Ident (s,_)) = "Ident [" ^ s ^ "]"
     | pp (Var (p,_)) = "Var [" ^ pp p ^ "]"
     | pp (TyId (p,_)) = "TyId [" ^ pp p ^ "]"
     | pp (Raise (p,_)) = "Raise [" ^ pp p ^ "]"
     | pp (LongId (l1,l2,_)) = "LongId [" ^ pp l1 ^ "." ^ pp l2 ^ "]"
     | pp (AndAlso (p1,p2,_)) = "AndAlso [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (OrElse (p1,p2,_)) = "OrElse [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (Seq (p1, p2,_)) = pp p1 ^ "\n" ^ pp p2
     | pp (Infix (a,b,c,_)) = "Infix [" ^ pp a ^ " " ^ pp b ^ " " ^ pp c ^ "]"
     | pp (OpExp (p,_)) = "Op [" ^ pp p ^ "]"
     | pp (App (p1,p2,_)) = "App [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (AppList (l,_)) = "AppList [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (TyAnn (p1,p2,_)) = "TyAnn [" ^ pp p1 ^ " : " ^ pp p2 ^ "]"
     | pp (TupleExp (l,p)) = "Tuple [" ^ (String.concatWith ", " 
                                             (map pp l)) ^ "]"
     | pp (UnitExp _) = "Unit []"
     | pp (SeqExp (l,_)) = "Seq [" ^ (String.concatWith "; " (map pp l)) ^ "]"
     | pp (ListExp (l,_)) = "List [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (NilExp p) = "Nil"
     | pp (LetExp (d,e,_)) = "Let [" ^ (String.concatWith "\n  " (map pp d)) ^ 
                            " in " ^ pp e ^ " end]\n"
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
     | pp (ImplicitPat (l, _)) = "[Implicit " ^ 
            String.concatWith ", " (map (fn (a,b) => pp a ^ " : " ^ pp b) l) ^
            "]"
     | pp (NilPat _) = "NilPat"
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
     | pp (TyPair (t1,_)) = "TyPair [" ^ String.concatWith " * " (map pp t1) ^ "]"
     | pp (TyCon (t1,t2,_)) = "TyCon [" ^ pp t1 ^ " " ^ pp t2 ^ "]"
     | pp (TyName (t1,_)) = pp t1
     | pp (TyClass (i1,t1,t2,p)) =
         "TyClass ["^ pp i1 ^ " " ^ pp t1 ^ " => " ^ pp t2 ^ "]"
     | pp (Match (p,e,_)) = "Match [" ^ pp p ^ " => " ^ pp e ^ "]\n"
     | pp (ValDec (b,_)) = 
         "ValDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]\n"
     | pp (TypeDec (b,_)) =
         "TypeDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]\n"
     | pp (InfixrDec (p,opr,_)) = 
         "Infixr [" ^ p ^ ", " ^ pp opr ^ "]\n"
     | pp (InfixDec (p,opr,_)) = 
         "Infix [" ^ p ^ ", " ^ pp opr ^ "]\n"
     | pp (DatatypeAssign (i1,i2,_)) = 
                     "DatatypeAssign [" ^ pp i1 ^ " = " ^ pp i2 ^ "]\n"
     | pp (DatatypeDec (d,_)) =
         "DatatypeDec [" ^ (String.concatWith "\nand " (map pp d)) ^ "]\n"
     | pp (FunDec (d,_)) = 
         "FunDec [" ^ (String.concatWith "\nand " 
            (map (fn l => String.concatWith "\n  | " (map pp l)) d)) ^ "]\n"
     | pp (ValBind (p,e,_)) = "ValBind [" ^ pp p ^ " = " ^ pp e ^ "]"
	  | pp (ValRec (b,_)) =
         "ValRec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]\n"
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
     | pp (DataBind (tv,i,b,_)) = "DataBind [(" ^ (String.concatWith "," 
                                                      (map pp tv)) ^
                                 ") " ^ pp i ^ " =\n   " ^ 
                                 (String.concatWith "\n | " (map pp b)) ^ "]"
     | pp (ConBind (i,NONE,_)) = "ConBind [" ^ pp i ^ "]"
     | pp (ConBind (i,SOME t,_)) = "ConBind [" ^ pp i ^ " of " ^ pp t ^ "]"
     | pp (StructDec (l,_)) = "Structure [" ^
                  (String.concatWith "\nand " (map pp l)) ^ "]\n"
     | pp (StructBind (i,a,b,_)) = "StructBind ["^ pp i ^ ppa a ^ " = "^
                                       pp b ^ "]" 
     | pp (Structure (l,_)) = "Struct [\n   " ^
                  (String.concatWith "\n   " (map pp l)) ^ "]"
     | pp (SigDec (s,_)) = 
         "SigDec [" ^ String.concatWith "\n  " (map pp s) ^ "]\n"
     | pp (SigBind (i,s,_)) = 
         "SigBind " ^ pp i ^ " = " ^ pp s ^ "\n"
     | pp (ClassDec (s,_)) = 
         "ClassDec [" ^ String.concatWith "\n  " (map pp s) ^ "]\n"
     | pp (ClassBind (i,t,_)) = 
         "ClassBind " ^ pp i ^ " to " ^ pp t ^ "\n" 
     | pp (Signature (d,_)) = "sig\n" ^ 
                        (String.concatWith "\n    " (map pp d)) ^ 
                        "\nend\n"
     | pp (ValDesc (v,_)) = "val " ^ (String.concatWith "\nand " (map pp v))
     | pp (TypeDesc (v,_)) = "type " ^ (String.concatWith "\nand " (map pp v))
     | pp (StructDesc (v,_)) = "structure " ^ 
         (String.concatWith "\nand " (map pp v))
     | pp (ValDescBind (i,t,_)) = pp i ^ " : " ^ pp t 
     | pp (StructDescBind (i,t,_)) = pp i ^ " : " ^ pp t
     | pp (TypeDescBind (t,i,_)) = String.concatWith "," (map pp t) ^ " " ^ 
                                       pp i
     | pp (ExnDec (i,NONE,_)) = "exception " ^ pp i ^ "\n"
     | pp (ExnDec (i,SOME t,_)) = "exception " ^ pp i ^ " of " ^ pp t ^ "\n"
     | pp (OpenDec (i,_)) = "open " ^ pp i ^ "\n"
     | pp (ClassImpl (i1,i2,i3,p)) = 
         "type " ^ pp i1 ^ " implements " ^ pp i2 ^ " in " ^ pp i3 ^ "\n"
     | pp (Import (i1,t,i2,_)) = "_import " ^ pp i1 ^ " : " ^ pp t ^ " as " 
                                 ^ pp i2 ^ "\n"
     

   and ppa (AscribeOpaque p) = " :> " ^ pp p
     | ppa (Ascribe p) = " : " ^ pp p
     | ppa Implicit = ""

   fun prettyPrint p = pp p
  
   fun prettyPrintAll l = String.concatWith "\n\n" (map prettyPrint l)

   fun getPos (Int (_, _, pos)) = pos
     | getPos (Real (_, _, _, _, pos)) = pos
     | getPos (Char (_, pos)) = pos
     | getPos (String (_, pos)) = pos
     | getPos (Ident (_, pos)) = pos
     | getPos (LongId (_, _, pos)) = pos
	  | getPos (TyId (_, pos)) = pos
     | getPos (Var (_, pos)) = pos
     | getPos (Raise (_, pos)) = pos
     | getPos (AndAlso (_, _, pos)) = pos
     | getPos (OrElse (_, _, pos)) = pos
     | getPos (Seq (_, _, pos)) = pos
     | getPos (Infix (_, _, _, pos)) = pos
     | getPos (OpExp (_, pos)) = pos
     | getPos (App (_, _, pos)) = pos
     | getPos (AppList (_, pos)) = pos
     | getPos (TyAnn (_, _, pos)) = pos
     | getPos (TupleExp (_, pos)) = pos
     | getPos (UnitExp pos) = pos
     | getPos (SeqExp (_, pos)) = pos
     | getPos (ListExp (_, pos)) = pos
     | getPos (NilExp pos) = pos
     | getPos (LetExp (_, _, pos)) = pos
     | getPos (IfExp (_, _, _, pos)) = pos
     | getPos (WhileExp (_, _, pos)) = pos
     | getPos (CaseExp (_, _, pos)) = pos
     | getPos (FnExp (_, pos)) = pos
     | getPos (HandleExp (_, _, pos)) = pos
     | getPos (ExnDec (_, _, pos)) = pos
     | getPos (OpenDec (_, pos)) = pos
     | getPos (Match (_, _, pos)) = pos
     | getPos (AsPat (_, _, pos)) = pos
     | getPos (UnitPat pos) = pos
     | getPos (InfixPat (_, _, _, pos)) = pos
     | getPos (WildcardPat pos) = pos
     | getPos (TuplePat (_, pos)) = pos
     | getPos (ListPat (_, pos)) = pos
     | getPos (NilPat pos) = pos
     | getPos (ConPat (_, pos)) = pos
     | getPos (ConstrPat (_, _, pos)) = pos
     | getPos (TyAnnPat (_, _, pos)) = pos
     | getPos (IdPat (_, pos)) = pos
     | getPos (TyArrow (_, _, pos)) = pos
     | getPos (TyPair (_, pos)) = pos
     | getPos (TyCon (_, _, pos)) = pos
     | getPos (TyName (_, pos)) = pos
     | getPos (TyClass (_, _, _, pos)) = pos
     | getPos (ValDec (_, pos)) = pos
     | getPos (TypeDec (_, pos)) = pos
     | getPos (InfixrDec (_, _, pos)) = pos 
     | getPos (InfixDec (_, _, pos)) = pos
     | getPos (DatatypeAssign (_, _, pos)) = pos
     | getPos (DatatypeDec (_, pos)) = pos
     | getPos (FunDec (_, pos)) = pos
     | getPos (ValBind (_, _, pos)) = pos
	  | getPos (ValRec (_, pos)) = pos
     | getPos (TypeBind (_, _, _, pos)) = pos
     | getPos (FunMatch (_, _, _, _, pos)) = pos
     | getPos (DataBind (_, _, _, pos)) = pos
     | getPos (ConBind (_, _, pos)) = pos
     | getPos (StructDec (_, pos)) = pos
     | getPos (StructBind (_, _, _, pos)) = pos
     | getPos (Structure (_, pos)) = pos
     | getPos (ValDesc (_, pos)) = pos
     | getPos (TypeDesc (_, pos)) = pos
     | getPos (StructDesc (_, pos)) = pos
     | getPos (ValDescBind (_, _, pos)) = pos 
     | getPos (TypeDescBind (_, _, pos)) = pos 
     | getPos (StructDescBind (_, _, pos)) = pos
     | getPos (SigBind (_, _, pos)) = pos
     | getPos (Signature (_, pos)) = pos
     | getPos (SigDec (_, pos)) = pos
     | getPos (ClassDec (_, pos)) = pos
     | getPos (ClassBind (_, _, pos)) = pos
     | getPos (ClassImpl (_, _, _, pos)) = pos
     | getPos (Import (_, _, _, pos)) = pos

   fun sanitise x = x
end

