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
    | VFn of (A.ast * A.ast * (string * value) list) list
    | VTuple of value list
    | VUnit
    | VBuiltIn of string

   val top_level_env = 
                 [("true",    VBool true),
                  ("false",   VBool false),
                  ("print",   VBuiltIn "print"),
                  ("nil",     VList []),
                  ("+",       VBuiltIn "+"),
                  ("-",       VBuiltIn "-"),
                  ("*",       VBuiltIn "*"),
                  ("/",       VBuiltIn "/"),
                  ("==",      VBuiltIn "=="),
                  ("!=",      VBuiltIn "!="),
                  ("<=",      VBuiltIn "<="),
                  (">=",      VBuiltIn ">="),
                  ("<",       VBuiltIn "<"),
                  (">",       VBuiltIn ">")
                 ] : (string * value) list 

   fun bind env (k,v) = (D.print 5 ("bind " ^ k ^ "\n"); (k,v) :: env)
   fun lookup env k = (case (List.find (fn (x,e) => k = x) (env)) 
                     of NONE => raise Fail ("Unknown id: " ^ k)
                      | SOME (_,v) => v)

   fun is_defined env k = (case (List.find (fn (x,e) => k = x) env) 
                     of NONE => false 
                      | SOME _ => true)

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

   fun tstr (VBool _) = "bool"
     | tstr (VInt _) = "int"
     | tstr (VReal _) = "real"
     | tstr (VString _) = "string"
     | tstr (VList []) = "'a list"
     | tstr (VList (h::t)) = tstr h ^ " list"
     | tstr (VTuple l) = String.concatWith " * " (List.map tstr l) 
     | tstr (VFn _) = "fn : ? -> ?"
     | tstr (VUnit) = "unit"
     | tstr (VBuiltIn s) = "_builtin " ^ s

   fun fvars (A.Identifier s) = [s]
     | fvars (A.FnExp _) = []
     | fvars (A.IfExp (a,b,c)) = fvars a @ fvars b @ fvars c
     | fvars (A.ListExp l) = List.foldl (op @) [] (List.map fvars l)
     | fvars (A.SeqExp l) = List.foldl (op @) [] (List.map fvars l)
     | fvars (A.TupleExp l) = List.foldl (op @) [] (List.map fvars l)
     | fvars (A.ConstantExp c) = []
     | fvars (A.AppExp (a,b)) = fvars a @ fvars b
     | fvars (A.InfixExp (a,b,c)) = fvars a @ fvars b @ fvars c
     | fvars (A.OrElseExp (a,b)) = fvars a @ fvars b
     | fvars (A.AndAlsoExp (a,b)) = fvars a @ fvars b
     | fvars (A.RaiseExp a) = fvars a
     | fvars (A.Pat a) = fvars a
     | fvars (A.WildcardPat) = []
     | fvars (A.InfixPat(a,b,c)) = fvars a @ fvars b @ fvars c
     | fvars (A.AsPat(a,b)) = fvars a @ fvars b
     | fvars (A.IdPat a) = fvars a
     | fvars (A.OpPat a) = fvars a
     | fvars (A.TuplePat l) = List.foldl (op @) [] (List.map fvars l)
     | fvars (A.ListPat l) = List.foldl (op @) [] (List.map fvars l)
     | fvars (A.ConstPat c) = []
     | fvars (A.TypePat (a,b)) = fvars a
     | fvars a
         = (Debug.print 6 ("Warning: free vars not collected inside: " ^ A.pp a); [])

   fun mk_env (p,e,env) =
      let
         val pv = fvars p
         val ev = fvars e

         fun fr [] l = l
           | fr (h::t) l = fr t (List.filter (fn x => x <> h) l)

         val free = fr pv ev

         val env' = List.foldr (fn (a,b) => (a,lookup env a) :: b) env free
      in
         env'
      end

   fun builtin env ("print",x) = (print ((vtostring x) ^ "\n"); VUnit)
     | builtin env ("+",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i+j)
                     | (VList i, VList j) => VList (i @ j)
                     | (VString i, x) => VString (i ^ vtostring x)
                     | _ => raise Fail ("applying + to invalid operands: " ^ 
                            tstr l ^ " * " ^ tstr r)
      end
     | builtin env ("-",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i-j)
                     | _ => raise Fail "applying - to invalid operands"
      end
     | builtin env ("/",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i div j)
                     | _ => raise Fail "applying / to invalid operands"
      end
     | builtin env ("*",t) =
      let
         val (VTuple [l,r]) = t
      in
         case (l,r) of (VInt i, VInt j) => VInt (i*j)
                     | _ => raise Fail "applying * to invalid operands"
      end
      | builtin env ("==",t) =
      let
         val (VTuple [l,r]) = t
      in
         VBool (is_equal (l,r))
      end
      | builtin env ("!=",t) =
      let
         val (VTuple [l,r]) = t
      in
         VBool (not (is_equal (l,r)))
      end
      | builtin env _ = raise NotImplemented "builtin"

   and pat_match env (A.WildcardPat, _) = (env, true)
     | pat_match env (A.AsPat (a,b), r) = 
       let
          val (env',t1) = pat_match env (a,r) 
          val (env'',t2) = pat_match env' (b,r)
       in
         (env'',t1 andalso t2)
       end
     | pat_match env (A.TuplePat l, VTuple r) =
       List.foldr (fn ((l,r),(e,t)) => 
       let
          val (env',t') = pat_match e (l,r)
       in
         (env',t' andalso t)
       end) (env,true) (ListPair.zip (l,r))
     | pat_match env (A.ListPat l, VList r) =
       List.foldr (fn ((l,r),(e,t)) => 
       let
          val (env',t') = pat_match e (l,r)
       in
         (env',t' andalso t)
       end) (env,true) (ListPair.zip (l,r))
     | pat_match env (A.ConstPat i, r) = 
       let
          val (env',v) = interp env i
       in
          (env',is_equal (v, r))
       end
     | pat_match env (A.TypePat _, _) = raise NotImplemented "Type Pattern"
     | pat_match env (A.Pat a, r) = pat_match env (a, r)
     | pat_match env (A.IdPat (A.Identifier i), r) = (bind env (i, r), true)
     | pat_match env (A.IdPat _, _) = raise NotImplemented "non-identifier id pat"
     | pat_match env _ = (env,false)

   and fn_apply env (l,r) =
      let
         val _ = D.print 6 "fn_apply\n"

         val (_,r') = interp env r

         fun ap [] = raise Fail "Match Exception"
           | ap ((p,e,env)::t) = 
             let
               val (env',m) = pat_match env (p,r')
               val env'' = if m then env' else env
             in
               if m then interp env'' e else ap t
             end
      in
         ap l
      end

   and interp env (A.ValDec l) = interp env (hd l) (* FIXME: ignore and clauses *)
     | interp env (A.ValBind (p,e)) = 
       let
          val (env',e') = interp env e
          val (env'',m) = pat_match env (p,e')
       in
          if m then (env'',VUnit) else raise Fail "Match exception"
       end
     | interp env (A.FunDec l) = interp env (hd l) (* FIXME: ignore and clauses *)
     | interp env (A.FunBind []) = raise Fail "fun def with no clauses"
     | interp env (A.FunBind l) = 
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

         val env' = bind env (id, VFn (List.map (fn (a,b) => (a,b,env)) m))
      in
         (env', VFn (List.map (fn (a,b) => (a,b,env)) m))
      end
     | interp env (A.Int (false,i)) = (env,VInt i)
     | interp env (A.Int (true,i)) = (env,VInt (~i))
     | interp env (A.String s) = (env,VString s)
     | interp env A.Unit = (env,VUnit)
     | interp env (A.TupleExp l) =
       (fn (f,lar) => (f,VTuple lar))
         (List.foldr (fn (e,(env',l')) => 
            let
               val (env'',e') = interp env' e
            in
               (env'',e' :: l')
            end) (env,[]) l)
     | interp env (A.ListExp l) =
       (fn (f,lar) => (f,VList lar))
         (List.foldr (fn (e,(env',l')) => 
            let
               val (env'',e') = interp env' e
            in
               (env'',e' :: l')
            end) (env,[]) l)
     | interp env (A.ConstantExp c) = interp env c
     | interp env (A.Identifier l) = (env, lookup env l)
     | interp env (A.AppExp (l,r)) = 
      let
         val (env',l') = interp env l
         val (env'', r') = interp env r (* LHS environment should not affect RHS! *)
      in
         case l' of 
                   VFn m => fn_apply env (m,r) 
                 | VBuiltIn p => (env,builtin env (p,r'))
                 | _ => raise Fail "Applying non-function type"
      end
     | interp env (A.InfixExp (l,r,A.Null)) = interp env (A.AppExp (l,r))
     | interp env (q as (A.InfixExp (l,opr,r))) = (D.print 6 ("InfixExp: "^A.pp q^"\n"); 
         interp env (A.AppExp (opr,A.TupleExp [l,r])))
     | interp env (A.FnExp l) = (env, VFn 
         (map (fn (A.Match (m1,m2)) => (m1,m2,mk_env (m1,m2,env))) l))
     | interp env (A.IfExp (c,t,f)) =
       let
          val (_,VBool c') = interp env c
       in
          if c' then interp env t else interp env f
       end
     | interp env (A.Null) = (env,VUnit)
     | interp env e = raise NotImplemented (A.pp e)

   fun interpret_h env [] = ()
     | interpret_h env (h::t) = 
       let 
          val (env',r) = interp env h
       in
          interpret_h env' t
       end handle (NotImplemented m) => 
                       (D.print 1 ("Not Implemented: " ^ m ^ "\n"); ())

   fun interpret l = interpret_h top_level_env l
end


