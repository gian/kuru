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
*    Monomorphise TypedASTs, instantiating polymorphic functions and
*    resolving type classes to instances.
******************************************************************************)
structure Monomorphise =
struct
   structure A = TypedAST
   structure T = Type
   structure I = Intermediate
   structure S = Symbol

   val uninst = ref S.empty : I.ir S.table ref
   val inst = ref [] : (T.ty * S.symbol * S.symbol * I.ir) list ref

   val fr = ref 0
   fun fresh () = (fr := !fr + 1; S.symbol ("_mono" ^ Int.toString (!fr)))

   fun addUninst i =
      let
         val s = fresh ()
         val _ = uninst := S.enter (!uninst, s, i)
         val _ = Debug.print Debug.debug ("Monomorphise.addUninst: " ^ 
            S.name s ^ " = " ^ I.prettyPrint i ^ "\n")
      in
         s
      end

   fun addUninstS s i =
      let
         val _ = uninst := S.enter (!uninst, s, i)
      in
         s
      end

   fun addInst s t i =
      let
         val s' = 
            case 
               List.find (fn (t',s',_,_) => 
                  (T.equal t t') andalso s = s') (!inst)
            of
               NONE => 
               let
                  val s' = fresh ()
                  val _ = inst := (!inst) @ [(t,s,s',i)]
               in
                  s'
               end
             | SOME (_,_,s,_) => s

         val _ = Debug.print Debug.debug ("Monomorphise.addInst: " ^ 
                     S.name s ^ " = " ^ S.name s' ^ " == " ^ 
                     I.prettyPrint i ^ " (" ^ T.prettyPrint t ^ ")\n")
      in
         s'
      end

   (* FIXME: check symbol first, then type equality *)
   fun getInst s t =
      List.find (fn (t',s',_, _) => 
         (T.equal t t') andalso s = s') (!inst)

   fun getUninst s = 
      case S.look (!uninst, s) of 
         NONE => 
            Error.fail ("Monomorphise: Cannot find symbol '"^S.name s^"'",(0,""))
       | SOME i => i

   val aliases = ref [] : I.ir list ref
   fun addAlias i = aliases := !aliases @ [i]

   fun tymap (T.POLY i) t = [(T.POLY i, t)]
     | tymap (T.FN ([a],b)) (T.FN ([c],d)) = tymap a c @ tymap b d
     | tymap (T.TUPLE a) (T.TUPLE b) = 
         List.foldl (fn ((a,b),m) => m @ tymap a b) [] (ListPair.zip (a,b))
     | tymap (T.CONSTR (a,b)) (T.CONSTR (c,d)) =
         (List.foldl (fn ((a,b),m) => m @ tymap a b) [] (ListPair.zip (a,c))) @
            tymap b d
     | tymap x _ = [] 

   fun instantiate (f as I.N (I.FN, e, T.FN ([it],ot), m, p)) e2 =
   let
      val m = tymap it (I.getType e2)
      
      val f' = 
         List.foldl (fn ((l,r),f) => I.mapType (T.substinty l r) f) f m
   in
      f'
   end
     | instantiate (x as I.N (I.SELF s,_,t,m,p)) e2 =
         Error.bug ("Monomorphise.instantiate","SELF: " ^ I.prettyPrint x,p)
     | instantiate (I.N (I.IDENT s, _, T.FN ([it],ot), m, p)) e2 =
    (case S.name s of 
      "::" =>
      let
         val t2 = I.getType e2

         val it = T.TUPLE [T.POLY 0,T.CONSTR ([T.POLY 0],T.t_list)]
         val ot = T.CONSTR ([T.POLY 0],T.t_list)
         val ty = T.FN ([it],ot)

         val f = I.N (I.CONS, [], ty, m, p) 

         val m = tymap it t2

         val f' = 
            List.foldl (fn ((l,r),f) => I.mapType (T.substinty l r) f) f m
      in
         f'
      end
    | "@" =>
         I.N (I.IDENT s, [], 
            T.FN ([T.TUPLE [T.CONSTR ([T.POLY 0],T.t_list),
               T.CONSTR ([T.POLY 0],T.t_list),T.t_list]],
                     T.CONSTR ([T.POLY 0],T.t_list)), m, p)
    | _ =>
   let
      val t2 = I.getType e2

      val _ = Debug.print Debug.debug 
         ("Monomorphise.instantiate: " ^ S.name s ^ " : " ^ T.prettyPrint t2 ^ "\n")

      val i =
         case getInst s t2 of
            NONE => 
            let
               val f = getUninst s

               val _ = Debug.print Debug.debug
               ("Monomorphise.instantiate: Uninstantiated: " ^ 
                  I.prettyPrint f ^ "\n")

               val (it,ot) = 
                  case I.getType f of
                     T.FN ([it],ot) => (it,ot)
                   | _ => Error.bug ("Monomorphise.instantiate",
                                     "Invalid non-FN for uninstantiated fn",p)

               val m = tymap it t2
               val f' = 
                  List.foldl (fn ((l,r),f) => I.mapType (T.substinty l r) f) f m
            in
               case f of I.N (I.SELF s,_,t,m,p) =>
                  I.N (I.IDENT s, [], I.getType f', m, p) (* FIXME: Poly? *)
                | _ =>
                  if Polymorphism.isPolyTy (I.getType f) then
                     I.N (I.IDENT (addInst s t2 f'), [], I.getType f', [], p)
                  else I.N (I.IDENT s,[],I.getType f,[],p)
            end
          | SOME (t,s1,s2,i) =>
               I.N (I.IDENT s2, [], I.getType i, [], p)
   in
      i
   end)
     | instantiate e1 e2 = 
      Error.bug ("Monomorphise.instantiate", 
                 "Attempted to instantiate: " ^ I.prettyPrint e1, (0,""))

   (** TODO: Apply the value restriction here *)
   (** FIXME: Fix the type fields of all affected nodes. *)
   fun monomorphise prog =
   let
      fun mpat (A.PCONST (e,t,p)) = mexp e
        | mpat (A.PTUPLE (v,t,p)) =
      let
         val (v',t') = 
            Vector.foldl (fn (v,(e1,e2)) => 
               let
                  val v' = mpat v
                  val t' = I.getType v'
               in
                  (e1 @ [v'], e2 @ [t'])
               end) ([],[]) v
      in
         I.N (I.TUPLE, v', T.TUPLE t', [], p) 
      end
        | mpat (A.PCONSTR (i,NONE,t,p)) = I.N (I.CON i, [], t, [], p)
        | mpat (A.PCONSTR (i,SOME (pa,t'),t,p)) = I.N (I.CON i, [mpat pa], t,[], p)
        | mpat (A.PID (s,t,p)) = I.N (I.IDENT s, [], t, [], p)
        | mpat (A.PINFIX (p1,s,p2,t,p)) = 
            I.N (I.PINFIX s, [mpat p1, mpat p2], t, [], p)
        | mpat (A.PWILD p) = I.N (I.PWILD, [], T.UNKNOWN, [], p)
        | mpat (A.PNIL p) = I.N (I.NIL, [], T.t_list, [], p)
        | mpat (A.PUNIT p) = I.N (I.UNIT, [], T.t_unit, [], p)
        | mpat _ = Error.bug ("Monomorphise.mpat", "Invalid pattern", (0,""))

      and mexp (A.EID (s,t,p)) = I.N (I.IDENT s, [], t, [], p)
        | mexp (A.ETUPLE (v,t,p)) =
      let
         val v' = Vector.foldl (fn (v,e) => e @ [mexp v]) [] v
         val t' = map I.getType v'
      in
         I.N (I.TUPLE,v',T.TUPLE t',[],p) 
      end
        | mexp (A.EIF (a,b,c,t,p)) = 
      let
         val a' = mexp a
         val b' = mexp b
         val c' = mexp c
      in
         I.N (I.IF, [a',b',c'], I.getType b', [], p)
      end
        | mexp (A.ELIT (A.LINT (i,p), _, _)) = I.N (I.INT i,[],T.t_int,[],p)
        | mexp (A.ELIT (A.LREAL (i,p), _, _)) =  I.N (I.REAL i,[],T.t_real,[],p)
        | mexp (A.ELIT (A.LCHAR (i,p), _, _)) = I.N (I.CHAR i,[],T.t_char,[],p)
        | mexp (A.ELIT (A.LSTRING (i,p), _, _)) = 
            I.N (I.STRING i,[],T.t_string,[],p)
        | mexp (A.ELIT (A.LUNIT p, _, _)) = I.N (I.UNIT,[],T.t_unit,[],p)
        | mexp (A.EWHILE (a,b,t,p)) =
      let
         val a' = mexp a
         val b' = mexp b
      in
         I.N (I.WHILE, [a',b'], T.CONSTR([I.getType b'],T.t_list), [], p)
      end
        | mexp (f as A.EFN ([s],e,T.FN ([it],ot),p)) =
      let
         val f' = I.N (I.FN, 
                        [I.N (I.IDENT s, [], it, [], p),
                         mexp e],
                        T.FN ([it],ot),
                        [],
                        p)

         val f'' = 
            if Polymorphism.isPolymorphic f
               then I.N (I.IDENT (addUninst f'), [], T.FN ([it],ot), [], p)
               else f'

         val _ = Debug.print Debug.debug ("Monomorphise.mexp: f': " ^ 
                     I.prettyPrint f' ^ "\n")
         val _ = Debug.print Debug.debug ("Monomorphise.mexp: f'': " ^ 
                     I.prettyPrint f'' ^ "\n")
      in
         f''
      end
        | mexp (A.ECASE (e,v,t,pos)) =
      let
         val v' = 
            Vector.foldl (fn ((p,e),l) => 
               let
                  val p' = mpat p
                  val e' = mexp e
               in
                  l @ 
                     [I.N (I.MATCH, [mpat p, mexp e], I.getType e', [], pos)]
               end) [] v
      in
         I.N (I.CASE, mexp e :: v', t, [], pos)
      end
        | mexp (A.EAPP (e1,e2,t,p)) =
      let
         val e1' = mexp e1
         val e2' = mexp e2

         val _ = Debug.print Debug.debug
            ("Monomorphise.mexp: EAPP: " ^ 
               A.ppe e1 ^ " (" ^ A.ppe e2 ^ ")\n" ^
             "Mapped to: " ^ 
               I.prettyPrint e1' ^ " (" ^ I.prettyPrint e2' ^ ")\n")

         val e1'' = instantiate e1' e2'

         val t' = 
            case I.getType e1'' of
               T.FN (_,ot) => ot
             | _ => Error.bug ("Monomorphise","Invalid non-FN type in APP.",p)
      in
         I.N (I.APP, [e1'', e2'], t', [], p)
      end
        | mexp (A.ERAISE (e,p)) =
      let
         val e' = mexp e
      in
         I.N (I.RAISE, [mexp e], I.getType e', [], p)
      end
        | mexp (A.ESEQ (v,t,p)) =
      let
         val v' = 
            Vector.foldl (fn (e,l) => l @ [mexp e]) [] v
         val t' = I.getType (hd (List.rev v'))
      in
         I.N (I.SEQ, v', t', [], p)
      end
        | mexp (A.ECON (i,NONE,t,p)) = I.N (I.CON i,[],t,[],p)
        | mexp (A.ECON (i,SOME (e,t'),t,p)) = I.N (I.CON i,[mexp e],t,[],p)
        | mexp (A.ESTUB (t,p)) = I.N (I.STUB,[],t,[],p)
        | mexp (A.ENIL p) = I.N (I.NIL,[],T.t_list,[],p)
        | mexp x = Error.bug ("Monomorphise","Not implemented: " ^ A.ppe x,(0,"")) 

      fun mstm (A.DVAL (s,e,t,p)) = 
         let
            val _ = Debug.print Debug.debug ("Monomorphism.mstm: DVAL " ^ 
                        S.name s ^ " = " ^ A.ppe e ^ "\n")
         
            val _ = case t of 
               T.FN ([it],ot) => 
                  ignore (addUninstS s (I.N (I.SELF s,[],t,[],p)))
                | _ => ()

            val m = mexp e
            val t' = I.getType m

            val _ = Debug.print Debug.debug ("Monomorphism.mstm: DVAL " ^ 
                        S.name s ^ " = " ^ I.prettyPrint m ^ "\n")

            val _ = 
               case t' of T.FN _ =>
                  let
                     val m' = 
                        case m of I.N(I.IDENT s',_,_,_,_) => 
                           (addAlias (
                              I.N (I.ALIAS (s,s',t),[],t,[],p)); 
                              getUninst s')
                                | _ => m
                  in
                     ignore (addUninstS s m')
                  end
                  | _ => ()
         in
            I.N (I.BIND s, [m], t', [], p)
         end
        | mstm (A.DVALREC (a,t,p)) = mstm a 
        | mstm (A.DMUTUAL (v,p)) =
         let
            val v' = 
               Vector.foldl (fn (v,e) => e @ [mstm v]) [] v
         in
            I.N (I.MUTUAL, v', T.UNKNOWN, [], p)
         end
        | mstm (A.DIMPORT (s1,t as T.FN ([it],ot),s2,p)) = 
         let
            val _ = addInst s2 it (I.N (I.EXTERN s1,[],t,[],p)) 
         in
            I.N (I.IMPORT (s1,t,s2), [], t, [], p)
         end
        | mstm x = 
            Error.bug
               ("Monomorphise.mstm", "Unhandled: " ^ A.prettyPrint [x],(0,""))

      val prog' = map mstm prog

      (* FIXME: This is a hack. All this poly stuff should be gone. *)
      val prog'' =
         List.filter (fn x => not (Polymorphism.isPolyTy (I.getType x))) prog'

      val frags = map 
         (fn (t1,s1,s2,i) => I.N (I.BIND s2, [i], I.getType i, [], (0,""))) 
            (!inst)
   in
      I.N (I.PROGRAM, frags @ prog'', T.UNKNOWN, [], (0,""))
   end
end

