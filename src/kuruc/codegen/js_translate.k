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
structure JSTranslate =
struct
   structure S = Symbol
   structure T = Type
   structure I = Intermediate

   val j = String.concat
   val jc = String.concatWith ", "
   val jnl = String.concatWith "\n"
   val jeol = String.concatWith ";\n"
   val jsp = String.concatWith " "

   val toplev = ref [] : string list list ref
   fun addData s = toplev := !toplev @ [s]

   val toplevty = ref [] : string list list ref
   fun addType s = toplevty := !toplevty @ [s]

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

   val fnlk = ref S.empty : string S.table ref
   fun addFnLk (k,s) = fnlk := S.enter (!fnlk,k,s)
   fun lookupFn k = S.look (!fnlk,k)

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
               (map (fn x => "   " ^ tr_ty x ^ " " ^ (ix ()) ^ ";") t)
      in
         tn ^ " *" 
      end
     | tr_ty (T.CONSTR ([t1],t2)) = tr_ty t2 (* FIXME *) 
     | tr_ty (T.FN ([t1],t2)) = "closure_t *"
     | tr_ty (T.POLY _) = "void *"
     | tr_ty (T.UNKNOWN) = "void *"
     | tr_ty t =
      Error.bug ("CTranslate.tr_ty", "Not implemented: " ^ T.prettyPrint t, (0,""))

   fun tr_pat ex (I.N (I.INT i,_,_,_,_)) = (ex ^ " == " ^ Int.toString i,[])
     | tr_pat ex (I.N (I.CHAR c,_,_,_,_)) = (ex ^ " == '" ^ String.str c ^ "'",[])
     | tr_pat ex (I.N (I.STRING s,_,_,_,_)) = (ex ^ " == \"" ^ String.toCString s ^ "\"",[])
     | tr_pat ex (I.N (I.BOOL b,_,_,_,_)) = 
         (ex ^ " == " ^ (if b then "true" else "false"),[])
     | tr_pat ex (I.N (I.UNIT,_,_,_,_)) = ("true",[])
     | tr_pat ex (I.N (I.IDENT s,_,t,_,_)) = 
         ("true",["var " ^ S.name s ^ " = " ^ ex ^ ";"])
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

         val v' = ["if(false) { }\n"] @ 
         List.concat (
            List.map
            (fn p => 
               let
                  val i = ix ()
                  val (p',pi) = tr_pat (j [ex,"->",i]) p
               in
                  pi @
                 [j ["else if ((",p',")) {"],
                  j ["   ", rt," = true;"],
                 "}"]
               end) v)

      in
         (rt,
         [j ["var ",rt," = false;"]] @ v' @ [endLabel ^ ":"] @ [";"]) 
      end
     | tr_pat ex (I.N(I.PWILD,_,_,_,_)) = ("true",[])
     | tr_pat ex (I.N(I.NIL,_,_,_,_)) =
         (j [ex," == null"], [])
     | tr_pat ex (I.N(I.CON i,b,t,_,p)) =
      let
         val _ = Debug.print Debug.debug 
            ("tr_pat CON ty: " ^ T.prettyPrint t ^ "\n")

         val t' = tr_ty t

         val c1 = ex ^ ".i == " ^ Int.toString i

         val (b',bi) = 
            case b of [] => ("true",[])
                    | [e] => tr_pat (ex ^ ".data") e
                    | _ => 
                    Error.bug ("JSTranslate.tr_pat","Invalid CON",p)
      in
         (j ["(",c1," && ",b',")"], bi)
      end
     | tr_pat ex (I.N (I.PINFIX s,[a,b],t,m,p)) =
         (case S.name s of
            "::" =>
            let
               val failLabel = fresh()
               val (a',ai) = 
                  tr_pat (ex^"[0]") a
               val (b',bi) =
                  tr_pat (ex^".slice(1)") b
            in
               ("("^ex^" != null)",
               [j ["if (",ex," != null) {"]] @
               ai @
               bi @
               ["}"])
            end
          | n => Error.fail ("Unrecognised infix pattern '"^n^"'",p))
     | tr_pat ex (x as I.N (_,_,_,_,p)) =
         Error.bug ("JSTranslate.tr_pat", "Invalid pattern: " ^ I.prettyPrint x, p)

   and tr_exp (I.N (I.INT i,_,_,_,_)) = (Int.toString i,[])
     | tr_exp (I.N (I.CHAR c,_,_,_,_)) = ("'" ^ String.str c ^ "'",[])
     | tr_exp (I.N (I.STRING s,_,_,_,_)) = 
         ("\"" ^ String.toCString s ^ "\"",[])
     | tr_exp (I.N (I.REAL r,_,_,_,_)) = (Real.toString r,[])
     | tr_exp (I.N (I.BOOL b,_,_,_,_)) = (if b then "true" else "false",[])
     | tr_exp (I.N (I.UNIT,_,_,_,_)) = ("null",[])
     | tr_exp (I.N (I.IDENT s,_,_,_,_)) = 
        (case lookupFn s of NONE => (S.name s,[])
                          | SOME s' => (s',[]))
     | tr_exp (I.N (I.EXTERN s,_,_,_,_)) = (S.name s,[])
     | tr_exp (I.N (I.ENV (i,s),_,_,_,_)) = ("function () { /* Closure " ^ S.name s ^ " */ }",[])
     | tr_exp (I.N (I.SELF s, _, _, _, _)) = ("SELF",[])
     | tr_exp (I.N (I.TUPLE, v, t, _, _)) =
      let
         val rt = fresh ()
         val t' = tr_ty t
         val t'' = String.substring (t',0,size t' - 2)
         val v' = map tr_exp v

         val ind = ref 0

         val v'' = List.concat ( 
            map (fn (r,i) =>
               let
                  val ix = Int.toString (!ind)
                  val _ = ind := !ind + 1
               in
                  i @
                  [j [rt,".f",ix," = ",r,";"]]
               end) v')
      in
         (rt,
          [j ["var ",rt," = {};"]] @
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
         [j ["var ",rt,";"]] @
          ai @
         [j ["if (",a',") {"]] @
          (map (fn x => "   " ^ x) bi) @
         [j ["   ",rt, " = ", b', ";"],
          "} else {"] @
          (map (fn x => "   " ^ x) ci) @
         [j ["   ",rt, " = ", c', ";"]] @
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
                    [j ["else if (", pat', ") {"]] @
                     (map (fn x => "   " ^ x) expi) @
                    [j ["   ",rt, " = ", exp', ";"]] @
                     ["}"]
                  end
                | (I.N (_, _, _, _, p),_) =>
                     Error.bug ("CTranslate.tr_exp", "Invalid non-match", p))
               [] cases

      in
         (rt,
         [j ["var ", rt, ";"]] @ 
          ei @
         ["if(false) { }\n"] @
          pats @
         ["else { RAISE (\"Match\"); }"])
      end
     | tr_exp (I.N (I.APP, [I.N (I.CONS,_,_,_,_),I.N(I.TUPLE,[a,b],_,_,_)],t,_,_))=
      let
         val (a',ai) = tr_exp a
         val (b',bi) = tr_exp b

         val rt = fresh ()
      in
         (rt,
          ai @
          bi @
         [j ["var ",rt," = ",b',".unshift(",a',");"]])
      end
     | tr_exp (I.N (I.APP, [I.N (I.EQ,_,_,_,_),I.N(I.TUPLE,[a,b],_,_,_)],t,_,_))=
      let
         val (a',ai) = tr_exp a
         val (b',bi) = tr_exp b
      in
         ("("^a'^ " == " ^ b' ^ ")",
          ai @
          bi) 
      end
     | tr_exp (I.N (I.APP, [a, b], t, _, p)) =
      let
         val t' = tr_ty t

         val (a',ai) = tr_exp a
         val (b',bi) = tr_exp b

         fun detuple (n as I.N (_,c,T.TUPLE t, _, p)) =
         let
            val ixi = ref 0
            fun ix () = 
            let
               val i = !ixi
               val _ = ixi := !ixi + 1
            in
               ".f" ^ Int.toString i
            end
         in
            String.concatWith ", " 
               (map (fn x => b' ^ ix()) c)
         end
           | detuple _ = b'

         val (a'',b'') = 
            if getAlias (S.symbol a') then (a',detuple b)
            else (a', b')

         val rt = fresh()

         val rtl = 
            if t' = "void" then "" else 
               (j ["var ",rt, " = "])
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
         ("null", ei @
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
            case c of [] => ("null",[])
                    | [x] => tr_exp x
                    | _ => Error.bug ("Translate.tr_exp",
                              "Invalid constructor", p)
      in
         (rt,
         bi @
         [j ["var ", rt, " = {};"],
          j [rt,".i = ",Int.toString i,";"],
          j [rt,".data = ", b', ";"]])
      end
     | tr_exp (I.N (I.CLOSURE, f::env, T.FN ([it],ot), _, p)) =
      let
         val rt = fresh ()
         val (f',fi) = tr_exp f

         val ixi = ref 1 (* Field names in closures start from d1 *)
         fun ix () =
            let
               val i = !ixi
               val _ = ixi := !ixi + 1
            in
               "d" ^ Int.toString i
            end

         val types = map (tr_ty o I.getType) env
         val _ = ixi := 1

         val body =
         List.concat (
            map (fn e =>
               let
                  val (e',ei) = tr_exp e
               in
                  ei @ 
                  [j [rt,".",ix()," = ",e',";"]]
               end) env)
      in
         (rt,
         [j["var ",rt," = ",f',";"]])
      end
     | tr_exp (I.N (I.NIL, _, _, _, _)) = ("[]",[])
     | tr_exp (I.N (I.FN, [i,body], _, _, _)) =
        let
            val (s,_) = tr_exp i
            val (body',bt) = tr_exp body
        in
            ("(function (" ^ s ^ ") {" ^ jeol bt ^ ";\nreturn " ^ body' ^ ";\n})",[])
        end
     | tr_exp (x as I.N (_,_,_,_,p)) = 
      Error.bug ("CTranslate.tr_exp", "Not implemented: " ^ I.prettyPrint x,p)

   and tr_stm (I.N (I.BIND k, [I.N (I.IDENT s,_,_,_,_)], t, _, p)) =
      let
         val _ = addFnLk (k,S.name s)
      in
         []
      end
     | tr_stm (I.N (I.BIND s, [e], t, _, p)) =
      let
         val (e',ei) = tr_exp e
      in
         ei @
         [j ["var ", S.name s, " = ", e', ";"]]
      end
     | tr_stm (I.N (I.BIND s, [], _, _, _)) = []
     | tr_stm (I.N (I.MUTUAL, v, _, _, p)) = 
         List.concat (map tr_stm v)
     | tr_stm (I.N (I.IMPORT (ename, t as T.FN ([it],ot), iname), _, _, _, _)) =
      let
        (* val _ = addData 
            [(j ["extern ", tr_ty ot, " ", S.name ename, 
                  "(",tr_ty it,");"])] *)
      in
         []
      end
     | tr_stm 
         (I.N(I.FUNDEC s,[I.N(I.IDENT s',_,_,_,_),e],t as T.FN ([it],ot), _, p)) =
      let
         val (e',ei) = tr_exp e
         val f' = S.name s

         val _ = if getAlias (S.symbol ("closure"^f'^"_t")) then () else
               let
                  val ixi = ref 1 (* Field names in closures start from d1 *)
                  fun ix () =
                  let
                     val i = !ixi
                     val _ = ixi := !ixi + 1
                  in
                     "d" ^ Int.toString i
                  end
                  val env = ClosureConvert.getEnv s
                  val types = map (tr_ty o I.getType) env
               in
                    () 
	            end

         val _ = addData
           ([j ["var ",
                S.name s,
                " = function (",S.name s',") {"]] @
             	(map (fn x => "   " ^ x) ei) @
            	[j ["   return ", e', ";"],
             	"}"])
      in
         []
      end
     | tr_stm (I.N (I.ALIAS (k,s,t),_,_,_,_)) =
      let
         val _ = addAlias (k, S.name s)
      in
         []
      end
     | tr_stm (I.N (I.IMPORT _, _, _, _, _)) = []
     | tr_stm (I.N (I.PROGRAM, v, _, _, p)) =
         List.concat (map tr_stm v)
     | tr_stm (s as I.N (_,_,_,_,p)) = 
         Error.bug ("JSTranslate.tr_stm", "Unhandled: " ^ I.prettyPrint s, p)
   
   fun translate prog =
      let
         val prog' = tr_stm prog
      in
         (!toplevty @ !toplev, prog')
      end
end

