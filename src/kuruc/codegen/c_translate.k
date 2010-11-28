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
*    C code generator backend that generates C code from Kuru IR.
******************************************************************************)
structure CTranslate =
struct
   structure S = Symbol
   structure T = Type
   structure I = Intermediate

   val j = String.concat
   val jc = String.concatWith ", "
   val jnl = String.concatWith "\n"
   val jsp = String.concatWith " "

   val toplev = ref [] : string list list ref
   fun addData s = toplev := !toplev @ [s]

   val frs = ref 0
   fun fresh () = "tmp" ^ (Int.toString (frs := !frs + 1; !frs))

   val frt = ref 0
   fun freshTy () = "ty" ^ (Int.toString (frt := !frt + 1; !frt)) ^ "_t"

   val aliases = ref S.empty : string S.table ref
   fun addAlias (k,s) = aliases := S.enter (!aliases,k,s)
   fun getAlias k = 
      case S.look (!aliases,k) of
         NONE => false
       | SOME _ => true

   fun tr_ty (T.NAME s) = 
      (case S.name s of
         "int" => "int"
       | "string" => "kuru_string_t *"
       | "char" => "char"
       | "real" => "float"
       | "unit" => "kuru_unit_t"
       | "bool" => "kuru_bool_t"
       | "cvoid" => "void"
       | "list" => "kuru_list_t *"
       | n => "constr_t *")
     | tr_ty (T.TUPLE t) =
      let
         val tn = freshTy ()

         val ixi = ref 0
         fun ix () = 
            let
               val i = !ixi
               val _ = ixi := !ixi + 1
            in
               "f" ^ Int.toString i
            end

         val b = 
            String.concatWith ";\n   " 
               (map (fn x => tr_ty x ^ " " ^ (ix ()) ^ ";") t)

         val d = ["typedef struct {"] @
                  [b] @
                 ["} " ^ tn ^ ";"]

         val _ = addData d
      in
         tn
      end
     | tr_ty (T.CONSTR ([t1],t2)) = tr_ty t2 (* FIXME *) 
     | tr_ty (T.FN ([t1],t2)) = "closure_t *"
     | tr_ty (T.UNKNOWN) = "void *"
     | tr_ty t =
      Error.bug ("CTranslate.tr_ty", "Not implemented: " ^ T.prettyPrint t, (0,""))

   fun tr_pat ex (I.N (I.INT i,_,_,_,_)) = (ex ^ " == " ^ Int.toString i,[])
     | tr_pat ex (I.N (I.CHAR c,_,_,_,_)) = (ex ^ " == '" ^ String.str c ^ "'",[])
     | tr_pat ex (I.N (I.STRING s,_,_,_,_)) = 
         ("KSTRCMP(" ^ ex ^ ",\"" ^ String.toCString s ^ "\")",[])
     | tr_pat ex (I.N (I.BOOL b,_,_,_,_)) = 
         (ex ^ " == " ^ (if b then "TRUE" else "FALSE"),[])
     | tr_pat ex (I.N (I.UNIT,_,_,_,_)) = ("TRUE",[])
     | tr_pat ex (I.N (I.IDENT s,_,t,_,_)) = 
         ("TRUE",[tr_ty t ^ " " ^ S.name s ^ " = " ^ ex ^ ";"])
     | tr_pat ex (I.N (I.TUPLE, v, T.TUPLE t, _, _)) =
      let
         val rt = fresh ()
        
         val endLabel = fresh ()

         val ixi = ref 0
         fun ix () = 
            let
               val i = !ixi
               val _ = ixi := !ixi + 1
            in
               "f" ^ Int.toString i
            end

         val v' =
         List.concat (
            List.map
            (fn p => 
               let
                  val i = ix ()
                  val (p',pi) = tr_pat (j [ex,"->",i]) p
               in
                  pi @
                 [j ["if (!(",p',")) {"],
                  j [rt," = FALSE;"],
                  j ["goto ", endLabel,";"],
                 "}"]
               end) v)

      in
         (rt,
         [j ["kuru_bool_t ",rt," = TRUE;"]] @ v' @ [endLabel ^ ":"]) 
      end
     | tr_pat ex (I.N(I.PWILD,_,_,_,_)) = ("TRUE",[])
     | tr_pat ex (I.N(I.NIL,_,_,_,_)) =
         (j [ex," == NULL"], [])
     | tr_pat ex (I.N(I.CON i,b,_,_,p)) =
      let
         val c1 = ex ^ "->i == " ^ Int.toString i

         val (b',bi) = 
            case b of [] => ("TRUE",[])
                    | [e] => tr_pat (ex ^ "->data") e
                    | _ => 
                    Error.bug ("CTranslate.tr_pat","Invalid CON",p)
      in
         (j ["(",c1," && ",b',")"], bi)
      end
     | tr_pat ex (x as I.N (_,_,_,_,p)) =
         Error.bug ("Translate.tr_pat", "Invalid pattern: " ^ I.prettyPrint x, p)

   and tr_exp (I.N (I.INT i,_,_,_,_)) = (Int.toString i,[])
     | tr_exp (I.N (I.CHAR c,_,_,_,_)) = ("'" ^ String.str c ^ "'",[])
     | tr_exp (I.N (I.STRING s,_,_,_,_)) = 
         ("KSTRDUP(\"" ^ String.toCString s ^ "\")",[])
     | tr_exp (I.N (I.REAL r,_,_,_,_)) = (Real.toString r ^ "f",[])
     | tr_exp (I.N (I.BOOL b,_,_,_,_)) = (if b then "TRUE" else "FALSE",[])
     | tr_exp (I.N (I.UNIT,_,_,_,_)) = ("UNIT",[])
     | tr_exp (I.N (I.IDENT s,_,_,_,_)) = (S.name s,[])
     | tr_exp (I.N (I.EXTERN s,_,_,_,_)) = (S.name s,[])
     | tr_exp (I.N (I.ENV (i,s),_,_,_,_)) = ("*(env+"^ Int.toString i ^")",[])
     | tr_exp (I.N (I.TUPLE, v, t, _, _)) =
      let
         val rt = fresh ()
         val t' = tr_ty t
         val v' = map tr_exp v

         val ind = ref 0

         val v'' = List.concat ( 
            map (fn (r,i) =>
               let
                  val ix = Int.toString (!ind)
                  val _ = ind := !ind + 1
               in
                  i @
                  [j [rt,"->f",ix," = ",r,";"]]
               end) v')
      in
         (rt,
          [j [t',"* ",rt," = (",t',"*) KMALLOC(sizeof (",t',"));"]] @
          v'')
      end
     | tr_exp (I.N (I.IF,[a,b,c],t,_,_)) =
      let
         val rt = fresh ()
         val t' = tr_ty t
         val (a',ai) = tr_exp a
         val (b',bi) = tr_exp b
         val (c',ci) = tr_exp c
      in
         (rt,
         [j [t'," ",rt,";"]] @
          ai @
         [j ["if (",a',") {"]] @
          bi @
         [j [rt, " = ", b', ";"],
          "} else {"] @
          ci @
         [j [rt, " = ", c', ";"]] @
         ["}"])
      end
     | tr_exp (I.N (I.CASE, e::cases, t, _, _)) =
      let
         val rt = fresh ()
         val t' = tr_ty t

         val (e',ei) = tr_exp e

         val endLabel = fresh ()

         val pats =
            List.foldl (
               fn (I.N (I.MATCH, [pat,exp], _, _, _),pats) =>
                  let
                     val (pat',pati) = tr_pat e' pat
                     val (exp',expi) = tr_exp exp
                  in
                     pats @
                     pati @
                    [j ["if (", pat', ") {"]] @
                     expi @
                    [j [rt, " = ", exp', ";"]] @
                    [j ["goto ", endLabel, ";"],
                     "}"]
                  end
                | (I.N (_, _, _, _, p),_) =>
                     Error.bug ("CTranslate.tr_exp", "Invalid non-match", p))
               [] cases

      in
         (rt,
         [j [t', " ", rt, ";"]] @ 
          ei @
          pats @
         ["RAISE (\"Match\");",
          endLabel ^ ":"])
      end
     | tr_exp (I.N (I.APP, [a, b], t, _, p)) =
      let
         val t' = tr_ty t

         val (a',ai) = tr_exp a
         val (b',bi) = tr_exp b

         val (a'',b'') = 
            if getAlias (S.symbol a') then (a',b')
            else ("((closure_t *)"^a'^")->func",
                  a' ^ ", " ^ b')

         val rt = fresh()

         val rtl = 
            if t' = "void" then "" else 
               (j [t'," ",rt, " = "])
      in
         (rt,
          ai @
          bi @
         [j [rtl, a'',"(",b'',");"]])
      end
     | tr_exp (I.N (I.RAISE, [e], t, _, p)) =
      let
         val (e',ei) = tr_exp e
      in
         ("void", ei @
          [j ["RAISE(",e',");"]])
      end
     | tr_exp (I.N (I.SEQ, c, t, _, p)) =
      let
         val rt = ref "void" 

         fun et [] = Error.bug ("CTranslate.tr_exp","Sequence empty",p)
           | et [c] =
            let
               val (c',ci) = tr_exp c
               val _ = rt := c'
            in
               ci
            end
           | et (h::t) = ((fn (_,i) => i) (tr_exp h)) @ et t

         val ins = et c
      in
         (!rt,ins)
      end
     | tr_exp (I.N (I.LET, c, t, _, p)) =
      let
         val c' = List.rev c
         val e = hd c'
         val d = List.rev (tl c')
         val d' = List.concat (map tr_stm d)
         val (e',ei) = tr_exp e
      in
         (e',d' @ ei)
      end
     | tr_exp (I.N (I.CON i, c, t, _, p)) =
      let
         val rt = fresh ()
         val t' = tr_ty t

         val (b',bi) = 
            case c of [] => ("NULL",[])
                    | [x] => tr_exp x
                    | _ => Error.bug ("Translate.tr_exp",
                              "Invalid constructor", p)
      in
         (rt,
         bi @
         [j ["constr_t *", rt, " = (constr_t *) KMALLOC(sizeof(constr_t));"],
          j [rt,"->i = ",Int.toString i,";"],
          j [rt,"->data = (void *)", b', ";"]])
      end
     | tr_exp (I.N (I.CLOSURE, f::env, T.FN ([it],ot), _, p)) =
      let
         val rt = fresh ()
         val (f',fi) = tr_exp f

         (* FIXME: include body *)
      in
         (rt,
         [j["closure_t *",rt," = (closure_t *) KMALLOC (sizeof(closure_t));"],
          j[rt,"->func = ",f',";"]])
      end
     | tr_exp (I.N (I.NIL, _, _, _, _)) = ("NULL",[])
     | tr_exp (x as I.N (_,_,_,_,p)) = 
      Error.bug ("CTranslate.tr_exp", "Not implemented: " ^ I.prettyPrint x,p)

   and tr_stm (I.N (I.BIND s, [e], t, _, p)) =
      let
         val (e',ei) = tr_exp e
      in
         ei @
         [j [tr_ty t, " ", S.name s, " = ", e', ";"]]
      end
     | tr_stm (I.N (I.BIND s, [], _, _, _)) = []
     | tr_stm (I.N (I.MUTUAL, v, _, _, p)) = 
         List.concat (map tr_stm v)
     | tr_stm (I.N (I.IMPORT (ename, t as T.FN ([it],ot), iname), _, _, _, _)) =
      let
         val _ = addData 
            [(j ["extern ", tr_ty ot, " ", S.name ename, 
                  "(",tr_ty it,");"])]
      in
         []
      end
     | tr_stm 
         (I.N(I.FUNDEC s,[I.N(I.IDENT s',_,_,_,_),e],t as T.FN ([it],ot), _, p)) =
      let
         val (e',ei) = tr_exp e
         val _ = addData
           ([j [tr_ty ot,
                " ",
                S.name s,
                "(closure_t *env, ",
                tr_ty it," ",S.name s',
                ") {"]] @
             ei @
            [j ["return ", e', ";"],
             "}"])
      in
         []
      end
     | tr_stm (I.N (I.ALIAS (k,s,t),_,_,_,_)) =
      let
         val _ = addData ["#define " ^ S.name k ^ " " ^ S.name s]
         val _ = addAlias (k, S.name s)
      in
         []
      end
     | tr_stm (I.N (I.PROGRAM, v, _, _, p)) =
         List.concat (map tr_stm v)
     | tr_stm (s as I.N (_,_,_,_,p)) = 
         Error.bug ("Translate.tr_stm", "Unhandled: " ^ I.prettyPrint s, p)
   
   fun translate prog =
      let
         val prog' = tr_stm prog
      in
         (!toplev, prog')
      end
end
