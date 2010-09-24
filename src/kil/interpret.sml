structure Interpret =
struct
   structure A = Ast
   structure D = Debug

   exception NotImplemented of string

   type id = string

   datatype value = 
      VInt of int
    | VReal of real
    | VString of string
    | VChar of char
    | VBool of bool
    | VList of value list
    | VFn of (A.ast * A.ast) list
    | VTuple of value list
    | VUnit
    | VBuiltIn of string

   val env = ref ([("true",   VBool true),
                  ("false",   VBool false),
                  ("print",   VBuiltIn "print"),
                  ("nil",     VList []),
                  ("+",       VBuiltIn "+"),
                  ("-",       VBuiltIn "-"),
                  ("*",       VBuiltIn "*"),
                  ("/",       VBuiltIn "/")
                 ] : (string * value) list) 

   val envStack = ref [] : (string * value) list list ref

   fun pop_env () = (D.print 5 "POP\n"; env := hd (!envStack); envStack := tl (!envStack))

   fun push_env () = (D.print 5 "PUSH\n"; envStack := (!env) :: (!envStack))

   fun bind (k,v) = (D.print 5 ("bind " ^ k ^ "\n"); env := (k,v) :: (!env))
   fun lookup k = (case (List.find (fn (x,e) => k = x) (!env)) 
                     of NONE => raise Fail ("Unknown id: " ^ k)
                      | SOME (_,v) => v)

   fun is_equal (VInt a, VInt b) = a = b
     | is_equal (VString a, VString b) = a = b
     | is_equal (VChar a, VChar b) = a = b
     | is_equal (VBool a, VBool b) = a = b
     | is_equal (VTuple a, VTuple b) = length a = length b andalso
         List.all is_equal (ListPair.zip (a,b))
     | is_equal (VList a, VList b) = length a = length b andalso
         List.all is_equal (ListPair.zip (a,b))
     | is_equal (VUnit, VUnit) = true
     | is_equal (VFn _, _) = raise Fail "= called on function"
     | is_equal (_, VFn _) = raise Fail "= called on function"
     | is_equal _ = false


   fun vtostring (VBool true) = "true"
     | vtostring (VBool false) = "false"
     | vtostring (VInt i) = Int.toString i
     | vtostring (VString s) = s
     | vtostring (VList l) = "[" ^ String.concatWith ", " (map vtostring l) ^ "]"
     | vtostring (VTuple l) = "(" ^ String.concatWith ", " (map vtostring l) ^ ")"
     | vtostring (VFn _) = "fn : ? -> ?"
     | vtostring _ = "???"

   fun print_env () = (String.concatWith "\n" (List.map (fn (k,v) => k ^ ": "^ vtostring v) (!env))) ^ "\n"

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

   and pat_match (A.WildcardPat, _) = true
     | pat_match (A.AsPat (a,b), r) = pat_match (a,r) andalso pat_match(b,r)
     | pat_match (A.TuplePat l, VTuple r) = 
        length l = length r andalso List.all pat_match (ListPair.zip (l,r))
     | pat_match (A.ListPat l, VList r) =
        length l = length r andalso List.all pat_match (ListPair.zip (l,r))
     | pat_match (A.ConstPat i, r) = is_equal (interp i, r)
     | pat_match (A.TypePat _, _) = raise NotImplemented "Type Pattern"
     | pat_match (A.Pat a, r) = pat_match (a, r)
     | pat_match (A.IdPat (A.Identifier i), r) = (bind (i, r); true)
     | pat_match (A.IdPat _, _) = raise NotImplemented "non-identifier id pat"
     | pat_match _ = false

   and fn_apply (VFn l, r) =
      let
         val _ = D.print 6 "fn_apply\n"

         fun ap [] = raise Fail "Match Exception"
           | ap ((p,e)::t) = 
             let
               val _ = D.print 6 ("fn pre:\n" ^ print_env ())
               val _ = push_env ()
               val m = pat_match (p,r)
               val _ = if not m then pop_env () else ()
               val _ = D.print 6 ("fn pm:\n" ^ print_env ())
             in
               if m then 
                  let
                     val _ = push_env ()
                     val _ = D.print 6 ("fn pi:\n" ^ print_env ())
                     val res = interp e
                     val _ = D.print 6 ("fn ap:\n" ^ print_env ())
                     val _ = pop_env ()
                  in 
                     res 
                  end
                  else ap t
             end
      in
         ap l
      end

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
     | interp (A.FunBind []) = raise Fail "fun def with no clauses"
     | interp (A.FunBind l) = 
      let
         val m = map (fn (A.FunMatch (_,p,_,e)) => 
                      let
                         (* Turn curried args into nested anonymous functions *)
                         (* fun f x y = E --> fn x => fn y => E *)
                         val p' = hd p
                         val e' = List.foldr 
                                    (fn (x,b) => A.FnExp [A.Match (x,b)]) e (tl p)

                         val _ = Debug.print 6 ("FunBind: fn "^ A.pp p'^" => "^A.pp e' ^ "\n")
                      in
                         (p',e')
                      end) l

         val (A.FunMatch (A.Identifier id,_,_,_)) = hd l (* get the function name *)

         val _ = bind (id, VFn m)
      in
         VFn m 
      end
     | interp (A.Int (false,i)) = VInt i
     | interp (A.Int (true,i)) = VInt (~i)
     | interp (A.String s) = VString s
     | interp A.Unit = VUnit
     | interp (A.TupleExp l) = (D.print 6 "interpTupleExp\n"; VTuple (List.map interp l))
     | interp (A.ListExp l) = VList (List.map interp l)
     | interp (A.ConstantExp c) = interp c
     | interp (A.Identifier l) = lookup l
     | interp (A.AppExp (l,r)) = 
      let
         val l' = interp l
         val r' = interp r
      in
         case l' of 
                   VFn l => fn_apply (l',r') 
                 | VBuiltIn p => builtin(p,r')
                 | _ => raise Fail "Applying non-function type"
      end
     | interp (A.InfixExp (l,r,A.Null)) = interp (A.AppExp (l,r))
     | interp (q as (A.InfixExp (l,opr,r))) = (D.print 6 ("InfixExp: " ^ A.pp q ^ "\n"); 
         interp (A.AppExp (opr,A.TupleExp [l,r])))
     | interp (A.FnExp l) = VFn (map (fn (A.Match m) => m) l)
     | interp (A.Null) = VUnit
     | interp e = raise NotImplemented (A.pp e)

   fun interpret [] = ()
     | interpret (h::t) = (interp h; interpret t) handle (NotImplemented m) => 
               (print ("Not Implemented: " ^ m ^ "\n"); ())
end


