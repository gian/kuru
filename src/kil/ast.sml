structure Ast =
struct
   datatype ast =
      ValDec of ast list
    | ValBind of ast * ast
    | DatatypeBind of ast list
    | FunDec of ast list
    | Open of ast list
    | Nonfix of ast list
    | Infix of int option * ast list
    | Infixr of int option * ast list
    | ExceptionBind of ast list
    | LocalDec of ast list * ast list
    | ValRec of ast list
    | FunMatch of ast * ast list * ast option * ast
    | FunBind of ast list
    | TypeBind of ast list option * ast * ast
    | TypeDec of  ast list
    | ConBind of ast * ast option
    | ExnBind of ast * ast option
    | Pat of ast
    | WildcardPat
    | InfixPat of ast * ast * ast
    | AsPat of ast * ast
    | IdPat of ast
    | OpPat of ast
    | TuplePat of ast list
    | ListPat of ast list
    | ConstPat of ast
    | TypePat of ast * ast
    | FnTyp of ast * ast
    | PairTyp of ast * ast
    | ConsTyp of ast * ast
    | RaiseExp of ast
    | AndAlsoExp of ast * ast
    | OrElseExp of ast * ast
    | InfixExp of ast * ast * ast
    | AppExp of ast * ast
    | ConstantExp of ast
    | TupleExp of ast list
    | SeqExp of ast list
    | ListExp of ast list
    | LetExp of ast list * ast
    | IfExp of ast * ast * ast
    | WhileExp of ast * ast
    | CaseExp of ast * ast list
    | FnExp of ast list
    | LabelExp of ast
    | Identifier of string
    | TyVar of ast 
    | Int of bool * int
    | String of string
    | Unit
    | Type of ast
    | OpExp of ast list
    | Match of ast * ast
    | DataBind of ast option * ast * ast list
    | Null

   fun pp (ValDec l) = "val " ^ (String.concatWith "\nand " (map pp l)) ^ "\n"
     | pp (ValBind (a,b)) = pp a ^ " = " ^ pp b
     | pp (DatatypeBind l)= "datatype "^(String.concatWith "\n     and " (map pp l))^"\n"
     | pp (FunDec l) = "fun " ^ String.concatWith "\nand " (map pp l)
     | pp (FunBind l) = pp_l "\n  | " l
     | pp (Open l) = "open " ^ String.concatWith "." (map pp l)
     | pp (Nonfix l) = "nonfix ...\n"
     | pp (Infix l) = "infix ...\n"
     | pp (Infixr l) = "infix ...\n"
     | pp (LocalDec (a,b)) = "local\n" ^ (pp_l "\n" a) ^ "in\n" ^ (pp_l "\n" b) ^ "end\n"
     | pp (ValRec a) = "rec " ^ pp_l "\nand " a
     | pp (FunMatch (a,l,NONE,b)) = pp a ^ " " ^(pp_l " " l)^ " = " ^ pp b
     | pp (FunMatch (a,l,SOME t,b)) = pp a ^ " " ^(pp_l " " l)^ " : "^pp t^ " = " ^ pp b
     | pp (TypeDec l) = "type " ^ pp_l "\n and " l
     | pp (ConBind (a,NONE)) = pp a
     | pp (ConBind (a,SOME t)) = pp a ^ " of " ^ pp t
     | pp (Pat a) = pp a
     | pp (WildcardPat) = "_"
     | pp (InfixPat (a,b,c)) = pp a ^ pp b ^ pp c
     | pp (AsPat (a,b)) = pp a ^ " as " ^ pp b
     | pp (IdPat i) = pp i
     | pp (OpPat a) = "op " ^ pp a
     | pp (TuplePat l) = "(" ^ pp_l ", " l ^ ")"
     | pp (ListPat l) = "[" ^ pp_l ", " l ^ "]"
     | pp (ConstPat c) = pp c
     | pp (TypePat (e,t)) = pp e ^ " : " ^ pp t
     | pp (FnTyp (a,b)) = pp a ^ " -> " ^ pp b
     | pp (PairTyp (a,b)) = pp a ^ " * " ^ pp b
     | pp (ConsTyp (a,b)) = pp a ^ " " ^ pp b
     | pp (RaiseExp i) = "raise " ^ pp i
     | pp (AndAlsoExp (a,b)) = pp a ^ " andalso " ^ pp b
     | pp (OrElseExp (a,b)) = pp a ^ " orelse " ^ pp b
     | pp (InfixExp (a,i,b)) = pp a ^ " " ^ pp i ^ " " ^ pp b
     | pp (AppExp (a,b)) = pp a ^ " " ^ pp b
     | pp (ConstantExp c) = pp c
     | pp (TupleExp l) = "(" ^ pp_l ", " l ^ ")"
     | pp (SeqExp l) = "(" ^ pp_l "; " l ^ ")"
     | pp (ListExp l) = "[" ^ pp_l ", " l ^ "]"
     | pp (LetExp (d,e)) = "let\n" ^ pp_l "\n" d ^ "\nin\n" ^ pp e ^ "\nend"
     | pp (IfExp (a,b,c)) = "if " ^ pp a ^ " then " ^ pp b ^ " else " ^ pp c
     | pp (FnExp c) = "fn " ^ pp_l "\n | " c
     | pp (Identifier s) = s
     | pp (TyVar a) = "'" ^ pp a
     | pp (Int (true,i)) = "~" ^ Int.toString i
     | pp (Int (false,i)) = Int.toString i
     | pp (String s) = "\"" ^ s ^ "\""
     | pp (Type a) = pp a
     | pp (OpExp l) = "op " ^ pp_l "." l
     | pp (Match (a,b)) = pp a ^ " => " ^ pp b
     | pp Unit = "()"
     | pp Null = ""
     | pp _ = "???" 
  and pp_l d a = String.concatWith d (map pp a)





end
