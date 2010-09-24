structure Interpret =
struct
   structure A = Ast

   exception NotImplemented of string

   type id = string

   datatype value = 
      VInt of int
    | VReal of real
    | VString of string
    | VChar of char
    | VBool of bool
    | VList of value list
    | VFn of (id * A.ast) list
    | VTuple of value list
    | VUnit
    | VBuiltIn of string

   val env = ref ([("true",    VBool true),
                  ("false",   VBool false),
                  ("print",   VBuiltIn "print"),
                  ("nil",     VList []),
                  ("+",       VBuiltIn "+"),
                  ("-",       VBuiltIn "-"),
                  ("*",       VBuiltIn "*"),
                  ("/",       VBuiltIn "/")
                 ] : (string * value) list) 

   fun bind (k,v) = env := (k,v) :: (!env)
   fun lookup k = (case (List.find (fn (x,e) => k = x) (!env)) 
                     of NONE => raise Fail ("Unknown id: " ^ k)
                      | SOME (_,v) => v)

   fun vtostring (VBool true) = "true"
     | vtostring (VBool false) = "false"
     | vtostring (VInt i) = Int.toString i
     | vtostring (VString s) = s
     | vtostring (VList l) = "[" ^ String.concatWith ", " (map vtostring l) ^ "]"
     | vtostring (VTuple l) = "(" ^ String.concatWith ", " (map vtostring l) ^ ")"
     | vtostring _ = raise NotImplemented "unprintable value" 

   fun builtin ("print",x) = (print ((vtostring x) ^ "\n"); VUnit)
     | builtin ("+",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i+j)
                     | (VList i, VList j) => VList (i @ j)
                     | (VString i, x) => VString (i ^ vtostring x)
                     | _ => raise Fail "applying + to invalid operands"
      end
     | builtin ("-",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i-j)
                     | _ => raise Fail "applying - to invalid operands"
      end
     | builtin ("/",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i div j)
                     | _ => raise Fail "applying / to invalid operands"
      end
     | builtin ("*",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i*j)
                     | _ => raise Fail "applying * to invalid operands"
      end
      | builtin _ = raise NotImplemented "builtin"


   and interp (A.ValDec l) : value = (List.app (ignore o interp) l; VUnit)
     | interp (A.ValBind ((A.Pat(A.IdPat(A.Identifier s)),e))) = 
      let
         val e' = interp e
         val _ = bind (s,e')
      in
         VUnit
      end
     | interp (A.ValBind (A.Pat(A.WildcardPat),e)) = (interp e; VUnit) 
     | interp (A.FunDec l) = (List.app (ignore o interp) l; VUnit)
     | interp (A.Int (false,i)) = VInt i
     | interp (A.Int (true,i)) = VInt (~i)
     | interp (A.String s) = VString s
     | interp A.Unit = VUnit
     | interp (A.TupleExp l) = VTuple (List.map interp l)
     | interp (A.ListExp l) = VList (List.map interp l)
     | interp (A.ConstantExp c) = interp c
     | interp (A.Identifier l) = lookup l
     | interp (A.AppExp (l,r)) = 
      let
         val l' = interp l
         val r' = interp r
      in
         case l' of 
                   VFn l => raise NotImplemented "Function types not implemented"
                 | VBuiltIn p => builtin(p,r')
                 | _ => raise Fail "Applying non-function type"
      end
     | interp (A.InfixExp (l,r,A.Null)) = interp (A.AppExp (l,r))
     | interp (A.InfixExp (l,opr,r)) = 
         interp (A.AppExp (opr,A.TupleExp [l,r]))
     | interp (A.Null) = VUnit
     | interp e = raise NotImplemented (A.pp e)

   fun interpret [] = ()
     | interpret (h::t) = (interp h; interpret t) handle (NotImplemented m) => 
               (print ("Not Implemented: " ^ m ^ "\n"); ())
end


