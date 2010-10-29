(*******************************************************************************
*  The Kuru Programming Language Compiler Toolset (http://www.kuru-lang.org)
*  Copyright (C) 2010  Gian Perrone
*
*  This program is free software: you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*******************************************************************************
*  This file:
*    A typed AST into which ParseTree trees are transformed.
******************************************************************************)
structure TypedAST =
struct
	structure T = Type
   structure S = Symbol

	type pos = ParseTree.pos

   datatype tval = LINT of int * pos
                 | LREAL of real * pos
                 | LCHAR of char * pos
                 | LSTRING of string * pos
                 | LUNIT of pos

   datatype tpat = PCONST of texp * T.ty * pos
                 | PLIST of tpat vector * T.ty * pos
                 | PTUPLE of tpat vector * T.ty * pos
                 | PCONSTR of S.symbol * tpat option * T.ty * pos
                 | PID of S.symbol * T.ty * pos 
                 | PINFIX of tpat * S.symbol * tpat * T.ty * pos

   and      texp = ELIT of tval * T.ty * pos
	              | EID of S.symbol * T.ty * pos
					  | ELID of S.symbol list * T.ty * pos
                 | ELIST of texp vector * T.ty * pos
                 | ETUPLE of texp vector * T.ty * pos
                 | EIF of texp * texp * texp * T.ty * pos
                 | EWHILE of texp * texp * T.ty * pos
                 | EFN of S.symbol * texp * T.ty * pos
                 | ECASE of texp * (tpat * texp) vector * T.ty * pos
                 | EAPP of texp * texp * T.ty * pos
                 | EEXN of texp * T.ty * pos
                 | ERAISE of texp * pos
                 | EHANDLE of tpat * texp * T.ty * pos
                 | ESEQ of texp vector * T.ty * pos
                 | ELET of tast vector * texp * T.ty * pos
                 | EANN of texp * T.ty * pos
                 
   and      tast = DVAL of S.symbol * texp * T.ty * pos
                 | DVALREC of tast * T.ty * pos
                 | DMUTUAL of tast vector * pos
                 | DSTRUCTURE of S.symbol * tast vector * pos
                 | DSTRUCTASSIGN of S.symbol * S.symbol * pos
                 | DNULL of pos (* For things we've removed *)

   val ppty = T.pp
   fun vtol f = Vector.foldr (fn (a,b) => a::b) [] f

	fun getType (DVAL (_,_,t,_)) = t
	  | getType (DVALREC (_,t,_)) = t
	  | getType (DNULL _) = T.UNKNOWN
	  | getType (DSTRUCTURE _) = T.UNKNOWN
	  | getType _ = raise Fail "Can't getType for non DVAL or DVALREC"

	fun setType (DVAL (a,b,_,c)) t = DVAL (a,b,t,c)
	  | setType (DVALREC (a,_,b)) t = DVALREC (a,t,b)
	  | setType (DNULL p) t = DNULL p
	  | setType x t = x

   fun subst_ty_pat tn th (PCONST (e,t,p)) = 
            PCONST (subst_ty_exp tn th e, T.substinty tn th t, p)
     | subst_ty_pat tn th (PLIST (v,t,p)) =
            PLIST (Vector.map (subst_ty_pat tn th) v,
                   T.substinty tn th t, p)
     | subst_ty_pat tn th (PTUPLE (v,t,p)) =
            PTUPLE (Vector.map (subst_ty_pat tn th) v,
                   T.substinty tn th t, p)
     | subst_ty_pat tn th (PCONSTR (s,b,t,p)) = 
            PCONSTR (s, 
                     (case b of NONE => NONE
                              | SOME b' => SOME (subst_ty_pat tn th b')),
                     T.substinty tn th t,
                     p)
     | subst_ty_pat tn th (PID (s,t,p)) =
            PID (s, T.substinty tn th t, p)
     | subst_ty_pat tn th (PINFIX (a,opr,b,t,p)) =
            PINFIX (subst_ty_pat tn th a,
                    opr,
                    subst_ty_pat tn th b,
                    T.substinty tn th t,
                    p)

   and subst_ty_exp tn th (ELIT (tv,t,p)) =
            ELIT (tv, T.substinty tn th t, p)
     | subst_ty_exp tn th (EID (s,t,p)) =
            EID (s, T.substinty tn th t, p)
     | subst_ty_exp tn th (ELID (s,t,p)) =
            ELID (s, T.substinty tn th t, p)
     | subst_ty_exp tn th (ELIST (v,t,p)) =
            ELIST (Vector.map (subst_ty_exp tn th) v,
                   T.substinty tn th t, p)
     | subst_ty_exp tn th (ETUPLE (v,t,p)) =
            ETUPLE (Vector.map (subst_ty_exp tn th) v,
                   T.substinty tn th t, p)
     | subst_ty_exp tn th (EIF (e1,e2,e3,t,p)) =
            EIF    (subst_ty_exp tn th e1,
                    subst_ty_exp tn th e2,
                    subst_ty_exp tn th e3,
                    T.substinty tn th t,
                    p)
     | subst_ty_exp tn th (EWHILE (e1,e2,t,p)) =
            EWHILE (subst_ty_exp tn th e1,
                    subst_ty_exp tn th e2,
                    T.substinty tn th t,
                    p)
     | subst_ty_exp tn th (EFN (s,e,t,p)) =
            EFN (s,
                 subst_ty_exp tn th e,
                 T.substinty tn th t,
                 p)
     | subst_ty_exp tn th (ECASE (e,m,t,p)) =
            ECASE (subst_ty_exp tn th e,
                   Vector.map
                         (fn (a,b) => 
                             (subst_ty_pat tn th a,
                              subst_ty_exp tn th b))
                         m
                   ,
                   T.substinty tn th t,
                   p)
     | subst_ty_exp tn th (EAPP (e1,e2,t,p)) =
            EAPP (subst_ty_exp tn th e1,
                  subst_ty_exp tn th e2,
                  T.substinty tn th t,
                  p)
     | subst_ty_exp tn th (EEXN (e,t,p)) =
            EEXN (subst_ty_exp tn th e,
                  T.substinty tn th t,
                  p)
     | subst_ty_exp tn th (ERAISE (e,p)) =
            ERAISE (subst_ty_exp tn th e, p)
     | subst_ty_exp tn th (EHANDLE (pat,e,t,p)) =
            EHANDLE (subst_ty_pat tn th pat,
                     subst_ty_exp tn th e,
                     T.substinty tn th t,
                     p)
     | subst_ty_exp tn th (ESEQ (v,t,p)) =
            ESEQ (
               Vector.map (subst_ty_exp tn th) v,
               T.substinty tn th t,
               p)
     | subst_ty_exp tn th (ELET (l,e,t,p)) =
            ELET (
        Vector.map (subst_ty_dec tn th) l,
                    subst_ty_exp tn th e,
                    T.substinty tn th t,
                    p)
     | subst_ty_exp tn th (EANN (e,t,p)) =
            EANN (subst_ty_exp tn th e,
                  T.substinty tn th t,
                  p)
   and subst_ty_dec tn th (DVAL (s,e,t,p)) =
            DVAL (s,
                  subst_ty_exp tn th e,
                  T.substinty tn th t,
                  p)
     | subst_ty_dec tn th (DVALREC (d,t,p)) =
            DVALREC (subst_ty_dec tn th d,
                     T.substinty tn th t,
                     p)
     | subst_ty_dec tn th (DMUTUAL (v,p)) =
            DMUTUAL (Vector.map (subst_ty_dec tn th) v, p)
     | subst_ty_dec tn th (DSTRUCTURE (s,v,p)) =
            DSTRUCTURE (s,
                        Vector.map (subst_ty_dec tn th) v,
                        p)
     | subst_ty_dec tn th d = d 

   fun subst_ty tn th l = 
      map (subst_ty_dec tn th) l

   fun ppv (LINT(i,p)) = Int.toString i
     | ppv (LREAL(r,p)) = Real.toString r
     | ppv (LCHAR(c,p)) = "#\"" ^ String.str (c) ^ "\""
     | ppv (LSTRING(s,p)) = "\"" ^ s ^ "\""
     | ppv (LUNIT _) = "()"

   fun ppp (PCONST(e,t,p)) = ppe e
     | ppp (PLIST(v,ty,p)) = "[" ^ (String.concatWith "," (map ppp (vtol v))) ^ "]"
     | ppp (PTUPLE(v,ty,p)) = "(" ^ (String.concatWith "," (map ppp (vtol v))) ^ ")"
     | ppp (PCONSTR(s,NONE,ty,p)) = "???constr???"
     | ppp (PCONSTR(s,SOME cp,ty,p)) = "???constr???"
     | ppp (PID(s,t,p)) = S.name s  
     | ppp (PINFIX (a,b,c,t,p)) = "(" ^ ppp a ^ S.name b ^ ppp c ^ ")"

   and ppe (ELIT(tv,ty,p)) =  
                             ppv tv 
     | ppe (EID(s,ty,p)) = S.name s 
	  | ppe (ELID(s,ty,p)) = String.concatWith "." (map Symbol.name s)
     | ppe (ELIST(v,ty,p)) = "[" ^ (String.concatWith "," (map ppe (vtol v))) ^ "]"
     | ppe (ETUPLE(v,ty,p)) = "(" ^ (String.concatWith "," (map ppe (vtol v))) ^ ")"
     | ppe (EIF(e1,e2,e3,t,p)) = "if " ^ ppe e1 ^ " then " ^ ppe e2 ^ " else " ^ 
	  											ppe e3
	  | ppe (EFN(s,e,ty,p)) = "fn " ^ S.name s ^  
                             " => (" ^ ppe e ^ ")"
     | ppe (ECASE(e1,m,ty,p)) = "case " ^ ppe e1 ^ " of [\n" ^
                                (String.concatWith "\n" (map
                                                         (fn (p,e) => ppp p ^ " => " ^ ppe e) (vtol m)))
                                ^ "]\n"
     | ppe (EAPP(e1,e2,ty,p)) =  
                                ppe e1 ^ " (" ^ ppe e2 ^ ")" 
     | ppe (ELET (v,e,t,p)) = "let\n   " ^ prettyPrint (vtol v) ^ 
                              "in\n   " ^ ppe e ^ 
                              "\nend\n"
     | ppe _ = "???texp???"


   and prettyPrint [] = ""
     | prettyPrint ((DNULL _)::t) = prettyPrint t
     | prettyPrint ((DVAL(s,e,ty,p))::t) = "val " ^
                                        S.name s ^ " : " ^ ppty ty ^ " = " ^ 
                                        ppe e ^ "\n" ^ prettyPrint t
     | prettyPrint ((DVALREC(l,ty,p))::t) = "rec " ^ 
                                            (prettyPrint [l]) ^"\n"
                                            ^ prettyPrint t
     | prettyPrint ((DMUTUAL(v,p))::t) = "DMUTUAL [\n" ^ 
                                          prettyPrint (vtol v) ^ "]\n" ^ 
                                          prettyPrint t
	  | prettyPrint ((DSTRUCTURE (s,l,p))::t) = "DSTRUCTURE " ^ S.name s ^ " [\n" ^
	  																prettyPrint (vtol l)  ^ "]\n" ^
																	prettyPrint t
     | prettyPrint (_::t) = "???\n" ^ prettyPrint t
end


