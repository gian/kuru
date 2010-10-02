structure ParseTree =
struct

   datatype pt =
      Int of bool * string
    | Real of bool * string * string * (bool * string) option
    | Char of string
    | String of string
    | Ident of string
    | LongId of pt * pt
    | Var of pt
    | Raise of pt
    | AndAlso of pt * pt
    | OrElse of pt * pt
    | Seq of pt * pt
    | Infix of pt * pt * pt
    | OpExp of pt
    | App of pt * pt
    | TyAnn of pt * pt
    | TupleExp of pt list
    | UnitExp
    | SeqExp of pt list
    | ListExp of pt list
    | LetExp of pt list * pt
    | IfExp of pt * pt * pt
    | WhileExp of pt * pt
    | CaseExp of pt * pt list
    | FnExp of pt list
    | HandleExp of pt * pt list
    | Match of pt * pt
    | AsPat of pt * pt
    | UnitPat
    | InfixPat of pt * pt * pt
    | WildcardPat
    | TuplePat of pt list
    | ListPat of pt list
    | ConPat of pt
    | ConstrPat of pt * pt option
    | TyAnnPat of pt * pt
    | IdPat of pt
    | TyArrow of pt * pt
    | TyPair of pt * pt
    | TyCon of pt * pt
    | TyName of pt
    | ValDec of pt list
    | TypeDec of pt list
    | DatatypeAssign of pt * pt
    | DatatypeDec of pt list
    | FunDec of pt list list
    | ValBind of pt * pt
    | TypeBind of pt list * pt * pt
    | FunMatch of pt * pt list * pt option * pt
    | DataBind of pt list * pt * pt list
    | ConBind of pt * pt option
    | StructDec of pt list
    | StructBind of pt * pt
    | Structure of pt list

   fun pp (Int (b,s)) = "Int [" ^ (if b then "~" else "") ^ s ^ "]" 
     | pp (Real (b,s1,s2,NONE)) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^ "]" 
     | pp (Real (b,s1,s2,SOME (b2,s3))) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^
         "e" ^ (if b2 then "~" else "") ^ s3 ^ "]"
     | pp (Char s) = "Char [#\"" ^ s ^ "\"]"
     | pp (String s) = "String [\"" ^ s ^ "\"]"
     | pp (Ident s) = "Ident [" ^ s ^ "]"
     | pp (Var p) = "Var [" ^ pp p ^ "]"
     | pp (Raise p) = "Raise [" ^ pp p ^ "]"
     | pp (LongId (l1,l2)) = "LongId [" ^ pp l1 ^ "." ^ pp l2 ^ "]"
     | pp (AndAlso (p1,p2)) = "AndAlso [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (OrElse (p1,p2)) = "OrElse [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (Seq (p1, p2)) = pp p1 ^ "\n" ^ pp p2
     | pp (Infix (a,b,c)) = "Infix [" ^ pp a ^ " " ^ pp b ^ " " ^ pp c ^ "]"
     | pp (OpExp p) = "Op [" ^ pp p ^ "]"
     | pp (App (p1,p2)) = "App [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (TyAnn (p1,p2)) = "TyAnn [" ^ pp p1 ^ " : " ^ pp p2 ^ "]"
     | pp (TupleExp l) = "Tuple [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (UnitExp) = "Unit []"
     | pp (SeqExp l) = "Seq [" ^ (String.concatWith "; " (map pp l)) ^ "]"
     | pp (ListExp l) = "List [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (LetExp (d,e)) = "Let [" ^ (String.concatWith "; " (map pp d)) ^ 
                            " in " ^ pp e ^ " end]"
     | pp (IfExp (e1,e2,e3)) = "If [" ^ pp e1 ^ " then " ^ pp e2 ^ " else " ^
                                  pp e3 ^ "]"
     | pp (WhileExp (e1,e2)) = "While [" ^ pp e1 ^ " do " ^ pp e2 ^ "]"
     | pp (CaseExp (e1,m)) = "Case [" ^ pp e1 ^ " of " ^ 
                                 (String.concatWith " | " (map pp m)) ^ "]"
     | pp (FnExp m) = "Fn [" ^(String.concatWith " | " (map pp m)) ^ "]"
     | pp (HandleExp (e,m)) = "Handle [" ^ pp e ^ ", " ^
                                 (String.concatWith " | " (map pp m)) ^ "]"
     | pp (AsPat (p1,p2)) = "AsPat [" ^ pp p1 ^ " as " ^ pp p2 ^ "]"
     | pp (InfixPat (p1,p2,p3)) = "InfixPat [" ^ pp p1 ^ " " ^ pp p2 ^ 
                                     " " ^ pp p3 ^ "]"
     | pp (WildcardPat) = "WildcardPat []"
     | pp (TuplePat l) = "TuplePat [" ^ 
            (String.concatWith ", " (map pp l)) ^ "]"
     | pp (UnitPat) = "UnitPat []"
     | pp (ListPat l) = "ListPat [" ^ 
            (String.concatWith ", " (map pp l)) ^ "]"
     | pp (ConPat c) = "ConPat [" ^ pp c ^ "]"
     | pp (ConstrPat (i,NONE)) = "ConstrPat [" ^ pp i ^ "]"
     | pp (ConstrPat (i,SOME p)) = "ConstrPat ["^ pp i ^ " "^ pp p ^"]"
     | pp (TyAnnPat (i,p)) = "TyAnnPat ["^ pp i ^ " : "^ pp p ^"]"
     | pp (IdPat c) = "IdPat [" ^ pp c ^ "]"
     | pp (TyArrow (t1,t2)) = pp t1 ^ " -> " ^ pp t2
     | pp (TyPair (t1,t2)) = pp t1 ^ " * " ^ pp t2
     | pp (TyCon (t1,t2)) = pp t1 ^ " " ^ pp t2
     | pp (TyName t1) = pp t1
     | pp (Match (p,e)) = "Match [" ^ pp p ^ " => " ^ pp e ^ "]"
     | pp (ValDec (b)) = 
         "ValDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]"
     | pp (TypeDec (b)) =
         "TypeDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]"
     | pp (DatatypeAssign (i1,i2)) = 
                     "DatatypeAssign [" ^ pp i1 ^ " = " ^ pp i2 ^ "]"
     | pp (DatatypeDec d) =
         "DatatypeDec [" ^ (String.concatWith "\nand " (map pp d)) ^ "]"
     | pp (FunDec d) = 
         "FunDec [" ^ (String.concatWith "\nand " 
            (map (fn l => String.concatWith "\n  | " (map pp l)) d)) ^ "]"
     | pp (ValBind (p,e)) = "ValBind [" ^ pp p ^ " = " ^ pp e ^ "]"
     | pp (TypeBind (v,p,e)) = "TypeBind [" ^ (String.concatWith "," 
                                 (map pp v))
                                ^ " " ^ pp p ^ " = " ^ pp e ^ "]"
     | pp (FunMatch (i,l,NONE,e)) = "FunMatch [" ^ pp i ^ " " ^ 
                                    (String.concatWith " " (map pp l)) ^ 
                                    " = " ^ pp e ^ "]"
     | pp (FunMatch (i,l,SOME t,e)) = "FunMatch [" ^ pp i ^ " " ^ 
                                    (String.concatWith " " (map pp l)) ^ 
                                    " : " ^ pp t ^
                                    " = " ^ pp e ^ "]"
     | pp (DataBind (tv,i,b)) = "DataBind [(" ^ (String.concatWith "," (map pp tv)) ^
                                 ") " ^ pp i ^ " =\n   " ^ 
                                 (String.concatWith "\n | " (map pp b)) ^ "]"
     | pp (ConBind (i,NONE)) = "ConBind [" ^ pp i ^ "]"
     | pp (ConBind (i,SOME t)) = "ConBind [" ^ pp i ^ " of " ^ pp t ^ "]"
     | pp (StructDec l) = "Structure [" ^
                  (String.concatWith "\nand " (map pp l)) ^ "]"
     | pp (StructBind (i,b)) = "StructBind ["^ pp i ^" = "^ pp b ^ "]" 
     | pp (Structure l) = "Struct [\n   " ^
                  (String.concatWith "\n   " (map pp l)) ^ "]"

   fun prettyPrint p = pp p

end

