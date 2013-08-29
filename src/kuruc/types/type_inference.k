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

   type constraint = A.ty * A.ty * Position.t

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
            val _ = Debug.print Debug.debugmore ("Looking up short id: " ^ S.toString x ^ " in:\n" ^
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
         val (CEnv (VDec _, cl)) = 
            case lookup h (env::envs) of
               (CEnv (VDec b, cl)) => (CEnv (VDec b, cl))
             | _ => Error.fail ("Undeclared identifier: " ^ S.toString x, S.getPos x)

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
            val _ = Debug.print Debug.debugmore ("Looking up short id: " ^ S.toString x ^ " in:\n" ^
               prettyPrintTEnv (env::envs)^ "\n")
         in
	         (case M.get (tenv (!env)) x of
	            SOME (_, closure) => closure
	         |  NONE => lookupType x envs)
         end
      else let
         val (h,t) = (S.hd x, S.tl x)
         val _ = Debug.print Debug.debugmore ("Looking up: " ^ S.toString h ^ " in:\n" ^
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
   val tyInt = T.TySimple (S.fromString "int" (0, "__builtin__"))
   val tyReal = T.TySimple (S.fromString "real" (0, "__builtin__"))
   val tyChar = T.TySimple (S.fromString "char" (0, "__builtin__"))
   val tyString = T.TySimple (S.fromString "string" (0, "__builtin__"))
   val tyBool = T.TySimple (S.fromString "bool" (0, "__builtin__"))
   val tyUnit = T.TySimple (S.fromString "unit" (0, "__builtin__"))
   val tyExn = T.TySimple (S.fromString "exn" (0, "__builtin__"))

   fun newTopLevel () =
      let
         val env = newEnv ()
         val _ = putTEnv env (S.fromString "int" (0, "__builtin__")) 
            (CSimp (TDef tyInt))
         val _ = putTEnv env (S.fromString "string" (0, "__builtin__"))
            (CSimp (TDef tyString))
         val _ = putTEnv env (S.fromString "char" (0, "__builtin__"))
            (CSimp (TDef tyChar))
         val _ = putTEnv env (S.fromString "real" (0, "__builtin__"))
            (CSimp (TDef tyReal))
         val _ = putTEnv env (S.fromString "unit" (0, "__builtin__"))
            (CSimp (TDef tyUnit))
         val _ = putTEnv env (S.fromString "bool" (0, "__builtin__"))
            (CSimp (TDef tyBool))
         val _ = putVEnv env (S.fromString "=" (0, "__builtin__"))
            (CSimp (VDec (T.TyArrow (T.TyTuple [T.TyVar 0, T.TyVar 0], tyBool))))
      in
         env
      end

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

   val globalConstraints : constraint list ref = ref []

   fun ppconstr' [] = ""
     | ppconstr' ((t1, t2, p)::t) = A.ppty t1 ^ " : " ^ A.ppty t2 ^ "\n" ^
         ppconstr' t

   fun ppconstr () = ppconstr' (!globalConstraints)

   fun addConstr c = globalConstraints := (!globalConstraints) @ [c]

   fun typeMismatch (expected, actual, pos) =
      Error.fail ("Type mismatch:\n   Expected: " ^ A.ppty expected ^ "\n   " ^
                  "Actual: " ^ A.ppty actual, pos)

   (* The names of these things are confusingly backwards.
   *
   *  This basically takes the body of a structure and returns a signature,
   *  or a set of mutual declarations and returns the "forward" declarations of
   *  necesssary to make it work.
   *)
   fun decsToDef ((A.ValDec ((A.IdentifierPattern (symb, _, _), _), pos)) :: t) =
         T.ValDef (symb, freshTy (), pos) :: decsToDef t
     | decsToDef ((A.ValRecDec ((A.IdentifierPattern (symb, _, _), _), pos)) :: t) =
         T.ValDef (symb, freshTy (), pos) :: decsToDef t
     | decsToDef ((A.MutualDec (decls, pos)) :: t) =
         decsToDef decls @ decsToDef t
     | decsToDef ((A.TypeDec (ty, _, pos)) :: t) =
         T.TypeDef (ty, pos) :: decsToDef t
     | decsToDef (_ :: t) = decsToDef t
     | decsToDef [] = []
         

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
         let
            (* We just try instantiating these with tyvars and seeing if they
            * come out the same! *)

            (* FIXME: if t11 happens to be 'TySeq' ('a,'b) we need to do
            * something different here. *)
            val var = freshTy ()

            val t12' = T.substInTy t12 t11 var
            val t22' = T.substInTy t22 t11 var
         in
            tyImp PM (t12', E1) (t22', E2)
         end
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
      val pmap : (int * int) list ref = ref []
      fun get x = (case List.find (fn (y, _) => x = y) (!pmap) of
             NONE => let val k = !src
                in k before (src := k + 1; pmap := (x, k) :: !pmap) end
            | SOME (_, k) => k)
      fun aux (t as A.TyMeta x) = if k > x then t else A.TyVar (get x)
        | aux (A.TyApp (t1, t2)) = A.TyApp (aux (force t1), aux (force t2))
        | aux (A.TyArrow (t1, t2)) = A.TyArrow (aux (force t1), aux (force t2))
        | aux (A.TyTuple ts) = A.TyTuple (map (force o aux) ts)
        | aux t = t
   in aux t end


   fun infLiteral (A.Int _) = tyInt
     | infLiteral (A.Real _) = tyReal
     | infLiteral (A.Char _) = tyChar
     | infLiteral (A.String _) = tyString
     | infLiteral (A.Bool _) = tyBool
     | infLiteral (A.Unit) = tyUnit

   and infPat envs (p as (A.LiteralPattern (l, pos))) = (infLiteral l, p)
     | infPat (env::envs) (A.IdentifierPattern (symb, ty, pos)) =
         let
            val t = freshTy ()
            val _ = putVEnv env symb (CSimp (VDec t))
            val _ = addConstr (ty, t, pos)
         in
            (t, T.IdentifierPattern (symb, t, pos))
         end
     | infPat envs (A.TuplePattern (pats, _, pos)) =
         let
            val tpats = map (infPat envs) pats
            val (ts, pats') = ListPair.unzip tpats
         in
            (T.TyTuple ts, T.TuplePattern (pats', T.TyTuple ts, pos))
         end
     | infPat envs (A.ConstructorPattern (symb, NONE, _, pos)) =
         let
            (* This is ugly :-( *)
            val cty = case lookup symb envs of
               (CSimp (VDec t)) => t
             | _ => Error.fail ("Invalid constructor pattern: " ^ S.toString symb, pos)
         in
            (cty, T.ConstructorPattern (symb, NONE, cty, pos))
         end
     | infPat envs (A.ConstructorPattern (symb, SOME pat, _, pos)) =
         let
            (* This is even uglier :-( :-( :-( *)

            val (t1, t2) = case lookup symb envs of
               (CSimp (VDec (T.TyArrow (t1, t2)))) => (t1, t2)
             | _ => Error.fail ("Invalid constructor pattern: " ^ S.toString symb, pos)

            val (pt, pat') = infPat envs pat
         in
            (t2, T.ConstructorPattern (symb, SOME pat', t2, pos))
         end
     | infPat envs (A.AsPattern (pat1, pat2, _, pos)) =
         let
            val (pt1, pat1') = infPat envs pat1
            val (pt2, pat2') = infPat envs pat2

            val _ = Debug.print Debug.debug
               ("AsPattern1: " ^ T.pppat pat1' ^ "\n")

            val _ = Debug.print Debug.debug
               ("AsPattern2: " ^ T.pppat pat2' ^ "\n")

            (* val _ = solve envs (pt1, pt2) *)
         in
            (pt2, A.AsPattern (pat1', pat2', pt2, pos))
         end
     | infPat envs pat = 
      let
         val _ = Debug.print Debug.debug ("NOT IMPLEMENTED: " ^ A.pppat pat ^ "\n")
      in
         (freshTy (), pat)
      end

   and infExp env (literal as (A.LiteralExpr (l, p))) = 
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
         val t1' = instantiate (ref []) t1
         val tr = freshTy ()
         val _ = addConstr (T.TyArrow (t2, tr), t1', pos)
         val _ = addConstr (t1', t1, pos)
      in 
         (tr, T.ApplyExpr (e1', e2', tr, pos))
      end
     | infExp envs (A.IdentifierExpr (s, _, pos)) =
      let 
         val t = 
            case (lookup s envs) of
                 (CSimp (VDec t')) => t'
               | _ => Error.bug ("TypeInference.tyinfExp",
                  "Symbol not found: " ^ S.toString s, pos)
      in 
         (t, T.IdentifierExpr (s, t, pos))
      end
     | infExp envs (A.LetExpr (decls, exp, _, pos)) = 
      let 
         val env' : environment ref = newEnv () 
         val (_, decls') = inferTypes (env'::envs) decls
         val (t, exp') = infExp (env'::envs) exp
      in
         (t, T.LetExpr (decls', exp', t, pos))
      end
     | infExp envs (A.CaseExpr (exp, clauses, _, pos)) =
      let
         val (t, exp') = infExp envs exp

         val tx = freshTy ()

         val (constraints, clauses') = 
            List.foldl (fn ((pat, exp), (constr,cl)) => 
               let
                  (* FIXME: force pat = t *)
                  val env' = newEnv ()
                  val (pt, pat') = infPat (env'::envs) pat
                  val (et, exp') = infExp (env'::envs) exp
               in
                  (constr @ [(pt,t,pos),(et,tx,pos)], cl @ [(pat',exp')])
               end) ([],[]) clauses

         val _ = List.app addConstr constraints
      in
         (tx, T.CaseExpr (exp', clauses', tx, pos))
      end
     | infExp envs (A.RaiseExpr (exp, pos)) = 
      let
         val (t, exp') = infExp envs exp
         val _ = addConstr (t, tyExn, pos)
      in
         (freshTy (), T.RaiseExpr (exp', pos))
      end
     | infExp envs (A.IfExpr (e1, e2, e3, _, pos)) =
      let
         val (t1, e1') = infExp envs e1

         val _ = solve envs (tyBool, t1)

         val tx = freshTy ()
         val (t2, e2') = infExp envs e2
         val (t3, e3') = infExp envs e3

         val _ = addConstr (t2, tx, pos)
         val _ = addConstr (t3, tx, pos)
      in
         (tx, T.IfExpr (e1', e2', e3', tx, pos))
      end
     | infExp envs (A.ConstructorExpr (symb, NONE, _, pos)) =
      let
         (* TODO: Verify that this makes sense *)
         val t = case (lookup symb envs) of
                  (CSimp (VDec t)) => t
               | _ => Error.fail ("Symbol not found: " ^ S.toString symb, pos)
      in
         (t, T.ConstructorExpr (symb, NONE, t, pos))
      end
     | infExp envs (A.SeqExpr (exps, _, pos)) = 
      let
         fun seq [] ex = Error.fail ("Empty sequence expression", pos)
           | seq [s] ex = 
            let
               val (t, s') = infExp envs s
            in
               (t, T.SeqExpr (ex @ [s'], t, pos))
            end
           | seq (s::xs) ex =
            let
               val (t, s') = infExp envs s
            in
               seq xs (ex @ [s'])
            end
      in
         seq exps []
      end
     | infExp envs (A.TupleExtractExpr (i, exp, _, pos)) =
      let
         val (t', exp') = infExp envs exp
         val elTy =
            case t' of 
               T.TyTuple ts => 
                  if length ts > i then List.nth (ts, i - 1) else
                     Error.fail ("#" ^ Int.toString i ^ " applied to a tuple of size " ^
                        Int.toString (length ts), pos)
             | t' => freshTy ()
      in
         (elTy, T.TupleExtractExpr (i, exp', elTy, pos)) 
      end
     | infExp envs (A.AnnotationExpr (exp, ty, pos)) =
      let
         val (ty', exp') = infExp envs exp
         val _ = addConstr (ty', ty, pos)
      in
         (ty', T.AnnotationExpr (exp', ty', pos))
      end
     | infExp envs exp = raise Fail ("Not implemented: " ^ A.ppexp 0 exp)
     
     (* (tyUnit, T.LiteralExpr(T.Unit, (0, "fake"))) *)

   and mkForwardDec (A.ValDec ((A.IdentifierPattern (symb, _, _), _), _)) = [symb]
     | mkForwardDec (A.ValRecDec ((A.IdentifierPattern (symb, _, _), _), _)) = [symb]
     | mkForwardDec (A.StructureMod (symb, _, _)) = [symb]
     | mkForwardDec _ = []

   and infDec (env::envs) (A.ValDec ((A.IdentifierPattern (symb, _, pos), exp), pos')) =
      let 
         val _ = Debug.print Debug.debugmore ("Before:\n" ^ ppTySets () ^ "\n")
         val k = !tyvarCounter
         val (t, e') = infExp (env::envs) exp
         val t' = mkPoly k (force t)
         val _ = Debug.print Debug.debugmore ("After:\n" ^ ppTySets () ^ "\n")
         val _ = Debug.print Debug.debugmore ("ValDec: " ^ T.ppdec 0
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
         val _ = Debug.print Debug.debugmore ("Before:\n" ^ ppTySets () ^ "\n")
         val k = !tyvarCounter
         val t = freshTy ()
         val (t', e') = infExp (putVEnv env symb (CSimp (VDec t)) :: envs) e
         val _ = solve (env::envs) (force t, force t')
         val t'' = mkPoly k (force t')
         val _ = Debug.print Debug.debugmore ("After:\n" ^ ppTySets () ^ "\n")
         val _ = Debug.print Debug.debugmore ("ValRecDec: " ^ T.ppdec 0
         (A.ValRecDec((A.IdentifierPattern (symb, t'', pos), e'), pos')) ^ "\n")
      in
         (putVEnv env symb (CSimp (VDec t'')) :: envs, 
            T.ValRecDec ((T.IdentifierPattern (symb, t', pos), e'), pos'))
      end
     | infDec (env::envs) (A.MutualDec (decls, pos')) =
      let
         val defs = decsToDef decls
         val (envs',defs') = inferTypes (env::envs) defs

         val _ = Debug.print Debug.debug ("Defs: " ^ 
            T.prettyPrint defs' ^ "\n")

         val (env', decls') = inferTypes (env::envs) decls
      in
         (env::envs, A.MutualDec (decls', pos'))
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
     | infDec (env::envs) (A.StructureAssignMod (symb1, symb2, pos)) =
      let
         (* TODO: Make sure it's a struct and not anything else! *)

         val sEntry = lookup symb2 (env::envs)
         val _ = putVEnv env symb1 sEntry
      in
         (env::envs, A.StructureAssignMod (symb1, symb2, pos))
      end
     | infDec envs exp = (envs, exp)

   and inferTypes (envs : environment ref list) decls =
   let
      val tDecls = 
         List.foldl (fn (decl, (envs, decls)) =>
            let
               val _ = Debug.print Debug.debug ("DECL: " ^ A.prettyPrint [decl] ^ "\n")
               val (envs', decl') = infDec envs decl
               val _ = Debug.print Debug.debug ("DECL': " ^ A.prettyPrint [decl'] ^ "\n")
               val _ = print ("Constraints:\n" ^ ppconstr () ^ "\n")
            in
               (envs', decls @ [decl'])
            end) (envs, []) decls
   in
      tDecls
   end
end
