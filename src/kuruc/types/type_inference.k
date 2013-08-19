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
*  This file:
*    Infers types for an untyped AST.
*
*    This is based substantially upon the type checker for substandard ML
*    (https://github.com/finrod/ssml/), by Filip Sieczkowski.
******************************************************************************)

structure TypeInference =
struct
   structure S = Symbol
   structure A = Ast
   structure T = TypedAst
   structure P = ParseTree
   structure M = SymbolMap

   datatype cdecl = VDec of A.ty
                  | TDef of A.ty
                  | TDec

   datatype closure = 
      CEnv of cdecl * (closure M.map * closure M.map) ref
    | CSimp of cdecl

   type environment = closure M.map * closure M.map

   val tyvarCounter = ref 0

   fun indent 0 = ""
     | indent i = "   " ^ indent (i - 1)

   fun tenv (t,_) = t

   fun venv (_,v) = v

   fun ppenv ind ((symb, CEnv (VDec ty, ref (cl,_))) :: t) =
         indent ind ^ S.toString symb ^ " : " ^ T.ppty ty ^ "\n" ^
            ppenv (ind+1) (M.values cl) ^ "\n" ^ ppenv ind t
     | ppenv ind ((symb, CSimp (VDec ty)) :: t) =
         indent ind ^ S.toString symb ^ " : " ^ T.ppty ty ^ "\n" ^
            ppenv ind t
     | ppenv ind ((symb, CSimp (TDef ty)) :: t) =
         indent ind ^ "type " ^ S.toString symb ^ " = " ^ T.ppty ty ^ "\n" ^
            ppenv ind t
     | ppenv ind [] = ""

   fun prettyPrintEnv (e::envs) = ppenv 0 (M.values (venv (!e))) ^ 
         "\n" ^ prettyPrintEnv envs
     | prettyPrintEnv [] = ""

   fun prettyPrintTEnv (e::envs) = ppenv 0 (M.values (tenv (!e))) ^ 
         "\n" ^ prettyPrintTEnv envs
     | prettyPrintTEnv [] = ""

   fun lookup (x : S.t) (env::envs) =
      if not (S.isLong x) then
         let
            val _ = Debug.print Debug.debug ("Looking up short id: " ^ S.toString x ^ " in:\n" ^
               prettyPrintEnv (env::envs)^ "\n")
         in
	         (case M.get (venv (!env)) x of
	            SOME (_, closure) => closure
	         |  NONE => lookup x envs)
         end
      else let
         val (h,t) = (S.hd x, S.tl x)
         val _ = print ("Looking up: " ^ S.toString h ^ " in:\n" ^
               prettyPrintEnv (env::envs)^ "\n")
         val (CEnv (VDec _, cl)) = lookup h (env::envs)
         val _ = Debug.print Debug.debug ("Found " ^ S.toString h ^ ":\n")
         val _ = Debug.print Debug.debug (prettyPrintEnv (cl::envs)^ "\n")
      in
         lookup t ([cl])
      end
     | lookup x [] = 
       Error.fail ("Undeclared identifier " ^ S.toString x, S.getPos x)  

   fun lookupType (x : S.t) (env::envs) =
      if not (S.isLong x) then
         let
            val _ = Debug.print Debug.debug ("Looking up short id: " ^ S.toString x ^ " in:\n" ^
               prettyPrintTEnv (env::envs)^ "\n")
         in
	         (case M.get (tenv (!env)) x of
	            SOME (_, closure) => closure
	         |  NONE => lookupType x envs)
         end
      else let
         val (h,t) = (S.hd x, S.tl x)
         val _ = print ("Looking up: " ^ S.toString h ^ " in:\n" ^
               prettyPrintTEnv (env::envs)^ "\n")
         val (CEnv (VDec _, cl)) = lookupType h (env::envs)
         val _ = Debug.print Debug.debug ("Found " ^ S.toString h ^ ":\n")
         val _ = Debug.print Debug.debug (prettyPrintEnv (cl::envs)^ "\n")
      in
         lookupType t ([cl])
      end
     | lookupType x [] = 
       Error.fail ("Undeclared identifier " ^ S.toString x, S.getPos x) 


   fun putTEnv (env as (ref (tenv,venv))) symbol value = 
   let
      val _ = env := (M.put tenv symbol value, venv)
   in
      env
   end

   fun putVEnv (env as (ref (tenv,venv))) symbol value = 
   let
      val _ = env := (tenv, M.put venv symbol value)
   in
      env
   end

   fun newEnv () = ref (M.empty (), M.empty ())

   (* Built-in types *)
   val tyInt = T.TySimple (S.fromString "Int64.int" (0, "__builtin__"))
   val tyReal = T.TySimple (S.fromString "Real64.real" (0, "__builtin__"))
   val tyChar = T.TySimple (S.fromString "Char.char" (0, "__builtin__"))
   val tyString = T.TySimple (S.fromString "String.string" (0, "__builtin__"))
   val tyBool = T.TySimple (S.fromString "Bool.bool" (0, "__builtin__"))
   val tyUnit = T.TySimple (S.fromString "unit" (0, "__builtin__"))
   val tyExn = T.TySimple (S.fromString "exn" (0, "__builtin__"))

   val freshTy = T.freshTy
   type id = int

   structure UF = IUnionFind

   val tysets : (id * (A.ty UF.set)) list ref = ref [] 

   fun ppTySets () =
   let
      fun p [] = ""
        | p ((id,set)::t) = Int.toString id ^ ":\n" ^ (UF.toString T.ppty
        set) ^ p t
   in
      p (!tysets)
   end

   fun getSet i = (case List.find (fn (i', _) => i = i') (!tysets) of
         SOME (_, s) => s
       | NONE => let val ns = UF.new (A.TyMeta i)
            in ns before tysets := (i, ns) :: (!tysets)
            end)


   fun plookup xs k = (case List.find (fn (j, _) => j = k) xs of
			   SOME (_, v) => v
			 | NONE => raise Fail ("Unbound variable " ^ T.ppty (T.TyMeta k)))

    fun instantiate P (A.TyVar x) =
	   (plookup (!P) x handle _ => 
         let 
            val t = freshTy () 
         in 
            t before P := (x, t) :: !P 
         end)
      | instantiate P (A.TyApp (t1, t2)) =
         let 
            val t1' = instantiate P ( t1)
            val t2' = instantiate P ( t2)
         in 
            A.TyApp (t1', t2')
         end
      | instantiate P (A.TyArrow (t1, t2)) =
         let
            val t1' = instantiate P ( t1)
            val t2' = instantiate P ( t2)
         in
            A.TyArrow (t1', t2')
         end
      | instantiate P (A.TyTuple ts) = A.TyTuple (map (instantiate P) ts)
      | instantiate P (A.TyApp (t1,t2)) = 
         A.TyApp (instantiate P t1, instantiate P t2)
      | instantiate P (A.TyLam (t1,t2)) =
         let
            val k = freshTy ()
         in
            T.substInTy t2 t1 k
         end
      | instantiate _ t = t

   fun force (A.TyMeta x) = UF.find (getSet x)
     | force t = t

   fun forceAll t =
   let fun aux (A.TyApp (t1, t2)) = A.TyApp (forceAll t1, forceAll t2)
        | aux (A.TyArrow (t1, t2)) = A.TyArrow (forceAll t1, forceAll t2)
        | aux (A.TyTuple ts) = A.TyTuple (map forceAll ts)
        | aux t = t
   in aux (force t)
   end

   fun occursUF i t =
   (case t of
        A.TyArrow (t1, t2) => occursUF i (force t1) orelse occursUF i (force t2)
      | A.TyApp   (t1, t2) => occursUF i (force t1) orelse occursUF i (force t2)
      | A.TyTuple ts => List.exists (occursUF i) ts
      | A.TyMeta j => i = j
      | _ => false)

   fun eval envs (ty as (A.TySimple sym)) = 
         (case lookupType sym envs of
            (CSimp (TDec)) => (ty, envs)
          | (CSimp (TDef ty')) => (ty', envs)
          | (CEnv (TDef ty', env')) => eval (env'::envs) ty'
          | (CEnv (TDec, _)) => (ty, envs)
          | _ =>  Error.fail 
               ("Type '" ^ S.toString sym ^ "' is not found", S.getPos sym))
     | eval envs (ty as (A.TyApp (t1, t2))) =
         (case eval envs t1 of
              (T.TyLam (tv,tx), envs') =>
                  let
                     val (t2', env) = eval envs t2
                  in
                     (T.substInTy tx tv t2', envs)
                  end
            | _ => (ty, envs))
     | eval envs ty = (ty, envs)

   fun tyImp PM (t1, E1) (t2, E2) =
	(case (eval E1 t1, eval E2 t2) of
	     ((A.TySimple x, _), (A.TySimple y, _)) => 
         S.equals(S.fromString "exn" (0, "__builtin__"), x) orelse 
            S.equals(S.fromString "exn" (0, "__builtin__"), y) orelse S.equals (x, y)
	   | ((A.TyMeta x, _), (A.TyMeta y, _)) => x = y
	   | ((A.TyApp (t11, t12), E1), (A.TyApp (t21, t22), E2)) =>
	      tyImp PM (t11, E1) (t21, E2) andalso tyImp PM (t12, E1) (t22, E2)
	   | ((A.TyArrow (t11, t12), E1), (A.TyArrow (t21, t22), E2)) =>
	      tyImp PM (t11, E1) (t21, E2) andalso tyImp PM (t12, E1) (t22, E2)
      | ((A.TyTuple t1, E1), (A.TyTuple t2, E2)) =>
         List.all (fn (t1',t2') => tyImp PM (t1',E1) (t2',E2)) (ListPair.zip (t1,t2))
      | ((A.TyLam (t11, t12), E1), (A.TyLam (t21, t22), E2)) =>
            raise Fail ("TyLam: " ^ A.ppty t1 ^ " =/= " ^ A.ppty t2)
      | ((A.TyUnion (t1, c1), E1), (A.TyUnion (t2, c2), E2)) =>
            List.all (fn ((s1,NONE),(s2,NONE)) => S.equals (s1,s2)
                       | ((s1, SOME t1),(s2, SOME t2)) => 
                             S.equals (s1,s2)
                       | _ => false) (ListPair.zip (c1,c2))
	   | (c, (A.TyVar x, _)) => (case List.find (fn (y, _) => x = y) (!PM) of
					  SOME (_, c') => tyImp PM c c'
					| NONE => true before (PM := (x, c) :: (!PM)))
	   | ((t1,_),(t2,_)) => raise Fail ("Types " ^ A.ppty t1 ^ " and " ^ A.ppty t2 ^ " not equal"))

   fun subt t1 t2 env = tyImp (ref []) (t1, env) (t2, env)
   
   fun pickCanon _ (A.TyMeta _, t) = t
     | pickCanon _ (t, A.TyMeta _) = t
     | pickCanon D (t1, t2) =
   if subt t1 t2 D then t1
   else raise Fail ("Non-substitution union called on " ^ A.ppty t1 ^ " and " ^ A.ppty t2)


   fun solve D (A.TyMeta x, A.TyMeta y) =
         UF.union (pickCanon D) (getSet x) (getSet y)
     | solve D (A.TyMeta x, tr) =
         if occursUF x tr then
            raise Fail ("Circular type constraints: " ^ 
               A.ppty (A.TyMeta x) ^ " in " ^ A.ppty tr)
         else UF.union (pickCanon D) (getSet x) (UF.new tr)
     | solve D (tl, tr as A.TyMeta x) = solve D (tr, tl)
     | solve D (t1 as A.TySimple a, t2 as A.TySimple b) =
         if subt t1 t2 D then () 
            else raise Fail ("Type error: " ^ A.ppty t1 ^ " =/= " ^ A.ppty t2)
     | solve D (A.TyVar a, A.TyVar b) =
         if a = b then () else raise Fail ("Polymorphic unification")
     | solve D (A.TyApp (t1, c1), A.TyApp (t2, c2)) =
         (solve D (force t1, force t2);
          solve D (force c1, force c2))
     | solve D (A.TyArrow (t1, t2), A.TyArrow (t3, t4)) =
         (solve D (force t1, force t3); solve D (force t2, force t4))
     | solve D (A.TyTuple ts1, A.TyTuple ts2) =
         List.app (solve D) (ListPair.zip (ts1,ts2))
     | solve D (t1, t2) = raise Fail ("Type error: " ^ T.ppty t1 ^ " =/= " ^ T.ppty t2)

   fun solveList D xs = List.foldl (fn (c, ()) => solve D c) () xs

   fun mkPoly k t =
   let val src = ref 0
      val map : (int * int) list ref = ref []
      fun get x = (case List.find (fn (y, _) => x = y) (!map) of
             NONE => let val k = !src
                in k before (src := k + 1; map := (x, k) :: !map) end
            | SOME (_, k) => k)
      fun aux (t as A.TyMeta x) = if k > x then t else A.TyVar (get x)
        | aux (A.TyApp (t1, t2)) = A.TyApp (aux (force t1), aux (force t2))
        | aux (A.TyArrow (t1, t2)) = A.TyArrow (aux (force t1), aux (force t2))
        | aux t = t
   in aux t end


   fun infLiteral (A.Int _) = tyInt
     | infLiteral (A.Real _) = tyReal
     | infLiteral (A.Char _) = tyChar
     | infLiteral (A.String _) = tyString
     | infLiteral (A.Bool _) = tyBool
     | infLiteral (A.Unit) = tyUnit

   fun infExp env (literal as (A.LiteralExpr (l, p))) = 
      (infLiteral l, literal)
     | infExp envs (A.Struct (decls, _, pos)) =
      let
         val (env'::envs', decls') = inferTypes envs decls
         val t' = T.TySig () (* TODO: infer sig/kind *)
      in
         (t', T.Struct (decls', t', pos))
      end
     | infExp envs (A.TupleExpr (exps, _, pos)) =
      let
         val tyexps' = map (infExp envs) exps
         val (types, exps') = ListPair.unzip tyexps'
         val t = T.TyTuple types
      in
         (t, T.TupleExpr (exps', t, pos))
      end
     | infExp envs (A.FnExpr ([(T.LiteralPattern (T.Unit, pos), exp)], _, pos')) =
      let 
         val t = tyUnit
         val (t', exp') = infExp envs exp
         val tx = A.TyArrow (t, t')
      in
         (tx, A.FnExpr ([(T.LiteralPattern (T.Unit, pos), exp')], tx, pos'))
      end
     | infExp envs (A.FnExpr ([(A.IdentifierPattern (symb, _, pos), exp)], _, pos')) =
      let
         val t = freshTy ()
         val env' = newEnv () 
         val _ = putVEnv env' symb (CSimp (VDec t))
         val (t', exp') = infExp  (env' :: envs) exp
         val tx = A.TyArrow (t, t')
      in 
         (tx, T.FnExpr ([(T.IdentifierPattern (symb, t, pos), exp')], tx, pos'))
      end
     | infExp envs (A.ApplyExpr (e1, e2, _, pos)) =
      let
         val (t1, e1') = infExp envs e1
         val (t2, e2') = infExp envs e2
         val tr = freshTy ()
         val _  = solve envs (force t1, T.TyArrow (t2, tr))
      in 
         (tr, T.ApplyExpr (e1', e2', forceAll tr, pos))
      end
     | infExp envs (A.IdentifierExpr (s, _, pos)) =
      let 
         val t = 
            case (lookup s envs) of
                 (CSimp (VDec t')) => t'
               | _ => Error.bug ("TypeInference.tyinfExp",
                  "Symbol not found: " ^ S.toString s, pos)
         val t' = instantiate (ref []) (force t)
      in 
         (t', T.IdentifierExpr (s, t', pos))
      end
     | infExp envs (A.LetExpr (decls, exp, _, pos)) = 
      let 
         val env' : environment ref = newEnv () 
         val (_, decls') = inferTypes (env'::envs) decls
         val (t, exp') = infExp (env'::envs) exp
      in
         (t, A.LetExpr (decls', exp', t, pos))
      end
     | infExp envs exp = (tyUnit, T.LiteralExpr(T.Unit, (0, "fake")))

   and mkForwardDec (A.ValDec ((A.IdentifierPattern (symb, _, _), _), _)) = [symb]
     | mkForwardDec (A.ValRecDec ((A.IdentifierPattern (symb, _, _), _), _)) = [symb]
     | mkForwardDec (A.StructureMod (symb, _, _)) = [symb]
     | mkForwardDec _ = []

   and infDec (env::envs) (A.ValDec ((A.IdentifierPattern (symb, _, pos), exp), pos')) =
      let 
         val _ = Debug.print Debug.debug ("Before:\n" ^ ppTySets () ^ "\n")
         val k = !tyvarCounter
         val (t, e') = infExp (env::envs) exp
         val t' = mkPoly k (force t)
         val _ = Debug.print Debug.debug ("After:\n" ^ ppTySets () ^ "\n")
         val _ = Debug.print Debug.debug ("ValDec: " ^ T.ppdec 0
         (A.ValDec((A.IdentifierPattern (symb, t', pos), e'), pos')) ^ "\n")
      in 
         ((putVEnv env symb (CSimp (VDec t'))) :: envs, 
            T.ValDec (
               (
                  A.IdentifierPattern (symb, t', pos), 
                  e'
               ), 
               pos'
            )
         )
      end
     | infDec (env::envs) (A.ValRecDec ((A.IdentifierPattern (symb, _, pos), e), pos')) =
      let 
         val _ = Debug.print Debug.debug ("Before:\n" ^ ppTySets () ^ "\n")
         val k = !tyvarCounter
         val t = freshTy ()
         val (t', e') = infExp (putVEnv env symb (CSimp (VDec t)) :: envs) e
         val _ = solve (env::envs) (force t, force t')
         val t'' = mkPoly k (force t')
         val _ = Debug.print Debug.debug ("After:\n" ^ ppTySets () ^ "\n")
         val _ = Debug.print Debug.debug ("ValRecDec: " ^ T.ppdec 0
         (A.ValRecDec((A.IdentifierPattern (symb, t'', pos), e'), pos')) ^ "\n")
      in
         (putVEnv env symb (CSimp (VDec t'')) :: envs, 
            T.ValRecDec ((T.IdentifierPattern (symb, t', pos), e'), pos'))
      end
     | infDec envs (A.MutualDec (decls, pos')) =
      let
         val _ = ()
         (* val _ = List.app (fn ((s,t), decl) => 
                     solve env' (force t, force (T.getDecType decl))) stdecls *)
      in
         (envs, A.MutualDec (decls, pos'))
      end
     | infDec (env::envs) (A.StructureMod (symb, body, pos)) =
      let
         val env' : environment ref = newEnv () 
         val (t, e') = infExp (env'::env::envs) body
         val t' = t
         val _ = putVEnv env symb (CEnv (VDec t', env'))
         (* TODO: infKind *)
      in
         (env::envs, A.StructureMod (symb, e', pos))
      end
     | infDec (env::envs) (A.TypeDec (A.TySimple symb, t2, pos)) =
      let
         val _ = putTEnv env symb (CSimp (TDef t2))

         val t' = A.TySimple symb

         (* Bind the constructors in the value environment *)
         val _ = case t2 of 
            T.TyUnion (_, constrs) =>
               List.map (fn (symb, NONE) => 
                  putVEnv env symb (CSimp (VDec t'))
               | (symb, SOME t) => 
                  putVEnv env symb (CSimp (VDec (T.TyArrow (t, t'))))) constrs
          | _ => []
      in
         (env::envs, A.TypeDec (A.TySimple symb, t2, pos))
      end
     | infDec (env::envs) (A.TypeDec (A.TyApp (A.TySimple symb, t2), td, pos)) =
      let

         val (symb', td') = 
            case t2 of
               A.TyVar i => (symb, T.TyLam (A.TyVar i, td))
             | A.TySeq ts => (symb, T.TyLam (T.TySeq ts, td))
             | te => Error.fail ("Invalid type variable: " ^ A.ppty te, A.getPos symb)

         val _ = putTEnv env symb' (CSimp (TDef td'))
         
         val t' = T.TyApp (T.TySimple symb, t2)

         (* Bind the constructors in the value environment *)
         val _ = case td of 
            T.TyUnion (_, constrs) =>
               List.map (fn (symb, NONE) => 
                  putVEnv env symb (CSimp (VDec t'))
               | (symb, SOME t) => 
                  putVEnv env symb (CSimp (VDec (T.TyArrow (t, t'))))) constrs
          | _ => []
      in
         (env::envs, A.TypeDec (A.TyApp (A.TySimple symb, t2), td', pos))
      end

     | infDec envs exp = (envs, exp)

   and inferTypes (envs : environment ref list) decls =
   let
      val tDecls = 
         List.foldl (fn (decl, (envs, decls)) =>
            let
               val (envs', decl') = infDec envs decl
            in
               (envs', decls @ [decl'])
            end) (envs, []) decls
   in
      tDecls
   end
end
