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
*    Converts a parse tree to a typed Abstract Syntax Tree
******************************************************************************)

structure ParseTreeAstConverter =
struct
   structure S = Symbol
   structure A = TypedAst
   structure P = ParseTree
  
   val nextTyIndex = ref 0
   fun freshTy () =
         A.TyMeta (nextTyIndex := 1 + !nextTyIndex; !nextTyIndex)

   fun toType (P.TyName (ident, pos)) = 
         A.TySimple (S.fromString (idToSymbol ident) pos)
     | toType (P.Ident (ident, pos)) =
         A.TySimple (S.fromString ident pos)
     | toType (P.TyArrow (t1, t2, pos)) =
         A.TyArrow (toType t1, toType t2)
     | toType (P.TyPair (types, pos)) =
         A.TyTuple (map toType types)
     | toType (P.TyCon (t1, t2, pos)) =
         A.TyApp (toType t2, toType t1)
     | toType (P.Var (P.Ident (s, p), pos)) =
         A.TyVar (S.fromString s pos)
     | toType p = Error.bug ("ParseTreeAstConverter", 
                     "Unhandled type: " ^ P.prettyPrint p,
                     P.getPos p)

   and maybeTv [] t = t
     | maybeTv [x] t = A.TyApp (t, toType x)
     | maybeTv l t = A.TyApp (t, A.TySeq (map toType l))

   and constructorsToType ty l =
      A.TyUnion (ty,
         map (fn (P.ConBind (P.Ident (i, p), NONE, _)) =>
                  (S.fromString i p, NONE)
               | (P.ConBind (P.Ident (i, p), SOME args, _)) =>
                  (S.fromString i p, SOME (toType args))
               | p => Error.fail ("Invalid clause in datatype", P.getPos p)) l)

   (** Deals with patterns. *)
   and toPat (P.IdPat (P.Ident (s, pos), pos')) = 
         A.IdentifierPattern (
            S.fromString s pos,
            freshTy (),
            pos
         )
     | toPat (P.TuplePat (l, pos)) =
         A.TuplePattern (
            map toPat l,
            freshTy (),
            pos
         )
     | toPat (P.ConstrPat (identifier, NONE, pos)) =
         A.ConstructorPattern (
            S.fromString (idToSymbol identifier) pos,
            NONE,
            freshTy (),
            pos
         )
     | toPat (P.ConstrPat (identifier, SOME pat, pos)) =
         A.ConstructorPattern (
            S.fromString (idToSymbol identifier) pos,
            SOME (toPat pat),
            freshTy (),
            pos
         )
     | toPat (P.NilPat pos) =
         A.ConstructorPattern (
            S.fromString "nil" pos,
            NONE,
            freshTy (),
            pos
         )
     | toPat (P.InfixPat (lhs, P.Ident ("::", pos), rhs, pos')) =
         A.ConstructorPattern (
            S.fromString "::" pos,
            SOME (A.TuplePattern ([toPat lhs, toPat rhs], freshTy (), pos)),
            freshTy (),
            pos'
         )
     | toPat (P.UnitPat pos) = A.LiteralPattern (A.Unit, pos)
     | toPat (P.ConPat (literal, pos)) = 
         A.LiteralPattern (toLit literal, pos)
     | toPat (P.WildcardPat pos) =
         A.IdentifierPattern (
            S.fromString "FRESH_ID" pos,
            freshTy (),
            pos
         )
     | toPat (P.TyAnnPat (pat, ty, pos)) =
         A.AnnotationPattern (
            toPat pat,
            toType ty,
            pos
         )
     | toPat (P.AsPat (pat1, pat2, pos)) =
         A.AsPattern (
            toPat pat1,
            toPat pat2,
            freshTy (),
            pos
         )
     | toPat p = 
      Error.bug ("ParseTreeAstConverter", 
         "Invalid pattern: " ^ P.prettyPrint p,
         P.getPos p)
 
   and idToSymbol (P.LongId (h, t, p)) =
         idToSymbol h ^ "." ^ idToSymbol t
     | idToSymbol (P.Ident (s, pos)) = s
     | idToSymbol p = Error.bug ("ParseTreeAstConverter",
                              "Malformed long identifier", P.getPos p)

   (** Deals with literals. *)
   and toLit (P.Int (b, s, p)) =
      let
         val i = if b then IntInf.fromString ("~" ^ s)
            else IntInf.fromString (s)
      in
         A.Int (valOf i)
      end
     | toLit (P.Real (b, s1, s2, _, _)) =
         A.Real ((if b then "~" else "") ^ s1 ^ "." ^ s2)
     | toLit (P.Char (i, _)) = A.Char i
     | toLit (P.String (l, _)) = A.String (Vector.fromList l)
     | toLit (P.UnitExp _) = A.Unit
     | toLit p = Error.bug ("ParseTreeAstConverter",
                              "Malformed literal expression", P.getPos p)


   (** Deals with expressions. *)
   and toExp (P.UnitExp pos) = A.LiteralExpr (A.Unit, pos) 
     | toExp (P.SeqExp (exps, pos)) = 
         A.SeqExpr (map toExp exps, freshTy (), pos)
     | toExp (P.IfExp (e1, e2, e3, pos)) =
         A.IfExpr (toExp e1, toExp e2, toExp e3, freshTy (), pos)
     | toExp (P.WhileExp (e1, e2, pos)) =
         A.WhileExpr (toExp e1, toExp e2, freshTy (), pos)
     | toExp (P.App (e1, e2, pos)) =
         A.ApplyExpr (toExp e1, toExp e2, freshTy (), pos)
     | toExp (P.Ident (symbol, pos)) =
         A.IdentifierExpr (
            S.fromString symbol pos,
            freshTy (),
            pos
         )
     | toExp (lid as (P.LongId (h, t, pos))) =
      let
      in
         A.IdentifierExpr (
            S.fromString (idToSymbol lid) pos,
            freshTy (),
            pos
         )
      end
     | toExp (l as (P.Int _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (l as (P.Real _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (l as (P.Char _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (l as (P.String _)) = A.LiteralExpr (toLit l, P.getPos l)
     | toExp (P.Var (P.Ident (symbol, pos), pos')) =
         A.IdentifierExpr (
            S.fromString symbol pos,
            freshTy (),
            pos'
         )
     | toExp (P.TupleExp ([e], pos)) =
         toExp e
     | toExp (P.TupleExp (l, pos)) =
         A.TupleExpr (
            map toExp l,
            freshTy (),
            pos
         )
     | toExp (P.LetExp (decls, exp, pos)) =
         A.LetExpr (
            toDecl decls,
            toExp exp,
            freshTy (),
            pos
         )
     | toExp (P.Raise (exp, pos)) =
         A.RaiseExpr (toExp exp, pos)
     | toExp (P.NilExp pos) = 
         A.ConstructorExpr (
            S.fromString "nil" pos,
            NONE,
            freshTy (),
            pos
         )
     | toExp (P.TyAnn (exp, ty, pos)) =
         A.AnnotationExpr (
            toExp exp,
            toType ty,
            pos
         )
     | toExp (P.CaseExp (exp, clauses, pos)) =
         A.CaseExpr (
            toExp exp,
            map (fn (P.Match (pat, body, _)) =>
                     (toPat pat, toExp body)
                  | _ => Error.bug ("ParseTreeAstConverter",
                           "Invalid case clause", pos)) clauses,
            freshTy (),
            pos
         )
     | toExp (P.FnExp (clauses, pos)) =
         A.FnExpr (
            map (fn (P.Match (pat, body, _)) =>
                     (toPat pat, toExp body)
                  | _ => Error.bug ("ParseTreeAstConverter",
                           "Invalid case clause", pos)) clauses,  
            freshTy (),
            pos
         )
     | toExp (P.HandleExp (exp, clauses, pos)) =
         A.HandleExpr (
            toExp exp,
            map (fn (P.Match (p,b,_)) => (toPat p, toExp b)
                  | _ => Error.bug ("ParseTreeAstConverter",
                           "Invalid handle clause", pos)) clauses,
            freshTy (),
            pos
         )
     | toExp p = Error.bug ("ParseTreeAstConverter",
                            "Malformed expression: " ^
                            P.prettyPrint p,
                            P.getPos p)

   and toClause (P.ValBind (pat, exp, pos)) =
         ((toPat pat, toExp exp), pos)
     | toClause _ = 
         Error.bug("ParseTreeAstConverter", "Not implemented", (0, "fake"))

   and toFunClause (P.FunMatch (i, [pat], _, exp, pos)) =
         (toPat pat, toExp exp)
     | toFunClause (P.FunMatch (i, pats, _, exp, pos)) =
         (A.CurriedPattern (
            map toPat pats,
            freshTy (),
            pos), 
            toExp exp)
     | toFunClause p = 
         Error.bug("ParseTreeAstConverter", "Invalid fun clause", P.getPos p)


   and getBindingName [] = raise Fail "ParseTreeAstConverter.getBindingName"
     | getBindingName bindings =
      let
         val bindingName = ref ""
         fun upd ((P.FunMatch (P.Ident (i, _), _, _, _, pos))::t) =
            if ((!bindingName) <> "" andalso 
               (!bindingName) <> i) then
                  Error.fail ("Function clauses with different names", pos)
            else (bindingName := i; upd t)
           | upd [] = !bindingName
      in
         upd bindings
      end

   (** Deals with top-level declarations. *)
   and toDecl [] = []
     | toDecl ((P.ValDec ([binding], pos))::t) =
         A.ValDec (toClause binding) ::
            toDecl t
     | toDecl ((P.ValDec (bindings, pos))::t) =
         A.MutualDec (
            map (fn c => A.ValDec (toClause c)) bindings,
            pos
         ) :: toDecl t
     | toDecl ((P.FunDec ([binding], pos))::t) = 
         A.FunDec 
            (S.fromString (getBindingName binding) pos, 
            map toFunClause binding, 
            pos
         ) :: toDecl t
     | toDecl ((P.FunDec (bindings, pos))::t) = 
         A.MutualDec (
            map (fn binding => 
               A.FunDec (
                  S.fromString (getBindingName binding) pos, 
                  map toFunClause binding, 
                  pos
               )
            ) bindings,
            pos
         ) :: toDecl t
     | toDecl ((P.StructDec (l, pos))::t) =
         toDecl l @ toDecl t
     | toDecl ((P.SigDec (l, pos))::t) =
         toDecl l @ toDecl t
     | toDecl ((P.StructBind (P.Ident (ident, pos), ascr, P.Ident (ident', pos'), pos''))::t) =
         A.StructureAssignMod (
            S.fromString ident pos,
            S.fromString ident' pos', (* TODO: Functor application? *)
            pos''
         ) :: toDecl t
     | toDecl ((P.StructBind (P.Ident (ident, pos), ascr, body, pos'))::t) =
         A.StructureMod (
            Symbol.fromString ident pos,
            NONE,
            hd (toDecl [body]),
            pos') :: toDecl t
     | toDecl ((P.SigBind (P.Ident (ident, pos), body, pos'))::t) =
         A.SignatureMod (
            Symbol.fromString ident pos,
            hd (toDecl [body]),
            pos') :: toDecl t
     | toDecl ((P.Structure (decls, pos))::t) =
         A.StructMod (
            toDecl decls, 
            pos
         ) :: toDecl t
     | toDecl ((P.Signature (defs, pos))::t) =
         A.SigMod (
            [], 
            pos
         ) :: toDecl t
     | toDecl ((P.TypeDec ([binding], pos))::t) =
         A.TypeDec (
            (fn (P.TypeBind (tvars, ty1, ty2, pos')) => 
               (maybeTv tvars (toType ty1), toType ty2, pos)
              | _ => Error.bug ("ParseTreeAstConverter", 
                        "Invalid type binding", pos)) binding
         ) :: toDecl t
     | toDecl ((P.TypeDec (bindings, pos))::t) =
         A.MutualDec (
            map (fn (P.TypeBind (tvars, ty1, ty2, pos')) => 
                        A.TypeDec (maybeTv tvars (toType ty1), toType ty2, pos)
                  | _ => Error.bug ("ParseTreeAstConverter", 
                        "Invalid type binding", pos)) bindings,
            pos
         ) :: toDecl t
     | toDecl ((P.DatatypeDec ([P.DataBind (tvars, ty1, clauses, pos)], pos'))::t) =
         A.TypeDec (
            maybeTv tvars (toType ty1),
            constructorsToType (maybeTv tvars (toType ty1)) clauses,
            pos
         ) :: toDecl t
     | toDecl ((P.DatatypeDec (bindings, pos))::t) =
         A.MutualDec (
            map (fn (P.DataBind (tvars, ty1, clauses, pos)) =>
                     A.TypeDec (
                        maybeTv tvars (toType ty1),
                        constructorsToType (maybeTv tvars (toType ty1)) clauses,
                        pos
                     )
                  | (P.TypeBind (tvars, ty1, ty2, pos')) => (* Withtype *)
                     A.TypeDec (
                        maybeTv tvars (toType ty1), 
                        toType ty2, 
                        pos
                     )
                  | p => Error.fail 
                     ("Invalid datatype binding", P.getPos p)) bindings,
            pos
         ) :: toDecl t
     | toDecl (p::t) = toDecl t
     | toDecl (p::_) = 
         Error.bug ("ParseTreeAstConverter", P.prettyPrint p, P.getPos p)

   (** This needs to flatten a list of parse tree lists into a single AST *)
   fun convert program =
      let
         val flattened = List.concat program
      in
         toDecl flattened
      end
end

