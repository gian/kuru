structure Ast =
struct
   datatype ast =
      ValDec of ast list
    | ValBind of ast * ast
    | DatatypeBind of ast list
    | FunBind of ast list list
    | Open of ast list
    | Nonfix of ast list
    | Infix of int option * ast list
    | Infixr of int option * ast list
    | ExceptionBind of ast list
    | LocalDec of ast list * ast list
    | ValRec of ast list
    | FunMatch of ast * ast list * ast option * ast
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
    | Type of ast
    | OpExp of ast list
    | Match of ast * ast
    | DataBind of ast option * ast * ast list
    | Null

   

end
