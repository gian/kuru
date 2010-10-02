structure Error =
struct
   fun resolveErrorPos (filename,linePositions,pos,buf) =
      let
         val k = ref 0
         fun l [] = (!k,0)
           | l (h::t) = 
           if pos <= h then (!k,h-pos) else (k := 1 + !k; l t)
         
         val (line,col) = l linePositions
      in
         filename ^ ":" ^ Int.toString line ^ "." ^ Int.toString col
      end
end

structure ParseTree =
struct

   datatype pt =
      Int of bool * string
    | Real of bool * string * string * (bool * string) option
    | Char of string
    | String of string
    | Ident of string
    | LongId of pt * pt
    | Var of pt
    | Raise of pt
    | AndAlso of pt * pt
    | OrElse of pt * pt
    | Seq of pt * pt
    | Infix of pt * pt * pt
    | OpExp of pt
    | App of pt * pt
    | TyAnn of pt * pt
    | TupleExp of pt list
    | UnitExp
    | SeqExp of pt list
    | ListExp of pt list
    | LetExp of pt list * pt
    | IfExp of pt * pt * pt
    | WhileExp of pt * pt
    | CaseExp of pt * pt list
    | FnExp of pt list
    | HandleExp of pt * pt list
    | Match of pt * pt
    | AsPat of pt * pt
    | UnitPat
    | InfixPat of pt * pt * pt
    | WildcardPat
    | TuplePat of pt list
    | ListPat of pt list
    | ConPat of pt
    | ConstrPat of pt * pt option
    | TyAnnPat of pt * pt
    | IdPat of pt
    | TyArrow of pt * pt
    | TyPair of pt * pt
    | TyCon of pt * pt
    | TyName of pt
    | ValDec of pt list
    | TypeDec of pt list
    | DatatypeAssign of pt * pt
    | DatatypeDec of pt list
    | FunDec of pt list list
    | ValBind of pt * pt
    | TypeBind of pt list * pt * pt
    | FunMatch of pt * pt list * pt option * pt
    | DataBind of pt list * pt * pt list
    | ConBind of pt * pt option
    | StructDec of pt list
    | StructBind of pt * pt
    | Structure of pt list

   fun pp (Int (b,s)) = "Int [" ^ (if b then "~" else "") ^ s ^ "]" 
     | pp (Real (b,s1,s2,NONE)) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^ "]" 
     | pp (Real (b,s1,s2,SOME (b2,s3))) = 
      "Real [" ^ (if b then "~" else "") ^ s1 ^ "." ^ s2 ^
         "e" ^ (if b2 then "~" else "") ^ s3 ^ "]"
     | pp (Char s) = "Char [#\"" ^ s ^ "\"]"
     | pp (String s) = "String [\"" ^ s ^ "\"]"
     | pp (Ident s) = "Ident [" ^ s ^ "]"
     | pp (Var p) = "Var [" ^ pp p ^ "]"
     | pp (Raise p) = "Raise [" ^ pp p ^ "]"
     | pp (LongId (l1,l2)) = "LongId [" ^ pp l1 ^ "." ^ pp l2 ^ "]"
     | pp (AndAlso (p1,p2)) = "AndAlso [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (OrElse (p1,p2)) = "OrElse [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (Seq (p1, p2)) = pp p1 ^ "\n" ^ pp p2
     | pp (Infix (a,b,c)) = "Infix [" ^ pp a ^ " " ^ pp b ^ " " ^ pp c ^ "]"
     | pp (OpExp p) = "Op [" ^ pp p ^ "]"
     | pp (App (p1,p2)) = "App [" ^ pp p1 ^ ", " ^ pp p2 ^ "]"
     | pp (TyAnn (p1,p2)) = "TyAnn [" ^ pp p1 ^ " : " ^ pp p2 ^ "]"
     | pp (TupleExp l) = "Tuple [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (UnitExp) = "Unit []"
     | pp (SeqExp l) = "Seq [" ^ (String.concatWith "; " (map pp l)) ^ "]"
     | pp (ListExp l) = "List [" ^ (String.concatWith ", " (map pp l)) ^ "]"
     | pp (LetExp (d,e)) = "Let [" ^ (String.concatWith "; " (map pp d)) ^ 
                            " in " ^ pp e ^ " end]"
     | pp (IfExp (e1,e2,e3)) = "If [" ^ pp e1 ^ " then " ^ pp e2 ^ " else " ^
                                  pp e3 ^ "]"
     | pp (WhileExp (e1,e2)) = "While [" ^ pp e1 ^ " do " ^ pp e2 ^ "]"
     | pp (CaseExp (e1,m)) = "Case [" ^ pp e1 ^ " of " ^ 
                                 (String.concatWith " | " (map pp m)) ^ "]"
     | pp (FnExp m) = "Fn [" ^(String.concatWith " | " (map pp m)) ^ "]"
     | pp (HandleExp (e,m)) = "Handle [" ^ pp e ^ ", " ^
                                 (String.concatWith " | " (map pp m)) ^ "]"
     | pp (AsPat (p1,p2)) = "AsPat [" ^ pp p1 ^ " as " ^ pp p2 ^ "]"
     | pp (InfixPat (p1,p2,p3)) = "InfixPat [" ^ pp p1 ^ " " ^ pp p2 ^ 
                                     " " ^ pp p3 ^ "]"
     | pp (WildcardPat) = "WildcardPat []"
     | pp (TuplePat l) = "TuplePat [" ^ 
            (String.concatWith ", " (map pp l)) ^ "]"
     | pp (UnitPat) = "UnitPat []"
     | pp (ListPat l) = "ListPat [" ^ 
            (String.concatWith ", " (map pp l)) ^ "]"
     | pp (ConPat c) = "ConPat [" ^ pp c ^ "]"
     | pp (ConstrPat (i,NONE)) = "ConstrPat [" ^ pp i ^ "]"
     | pp (ConstrPat (i,SOME p)) = "ConstrPat ["^ pp i ^ " "^ pp p ^"]"
     | pp (TyAnnPat (i,p)) = "TyAnnPat ["^ pp i ^ " : "^ pp p ^"]"
     | pp (IdPat c) = "IdPat [" ^ pp c ^ "]"
     | pp (TyArrow (t1,t2)) = pp t1 ^ " -> " ^ pp t2
     | pp (TyPair (t1,t2)) = pp t1 ^ " * " ^ pp t2
     | pp (TyCon (t1,t2)) = pp t1 ^ " " ^ pp t2
     | pp (TyName t1) = pp t1
     | pp (Match (p,e)) = "Match [" ^ pp p ^ " => " ^ pp e ^ "]"
     | pp (ValDec (b)) = 
         "ValDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]"
     | pp (TypeDec (b)) =
         "TypeDec [" ^ (String.concatWith "\nand " (map pp b)) ^ "]"
     | pp (DatatypeAssign (i1,i2)) = 
                     "DatatypeAssign [" ^ pp i1 ^ " = " ^ pp i2 ^ "]"
     | pp (DatatypeDec d) =
         "DatatypeDec [" ^ (String.concatWith "\nand " (map pp d)) ^ "]"
     | pp (FunDec d) = 
         "FunDec [" ^ (String.concatWith "\nand " 
            (map (fn l => String.concatWith "\n  | " (map pp l)) d)) ^ "]"
     | pp (ValBind (p,e)) = "ValBind [" ^ pp p ^ " = " ^ pp e ^ "]"
     | pp (TypeBind (v,p,e)) = "TypeBind [" ^ (String.concatWith "," 
                                 (map pp v))
                                ^ " " ^ pp p ^ " = " ^ pp e ^ "]"
     | pp (FunMatch (i,l,NONE,e)) = "FunMatch [" ^ pp i ^ " " ^ 
                                    (String.concatWith " " (map pp l)) ^ 
                                    " = " ^ pp e ^ "]"
     | pp (FunMatch (i,l,SOME t,e)) = "FunMatch [" ^ pp i ^ " " ^ 
                                    (String.concatWith " " (map pp l)) ^ 
                                    " : " ^ pp t ^
                                    " = " ^ pp e ^ "]"
     | pp (DataBind (tv,i,b)) = "DataBind [(" ^ (String.concatWith "," (map pp tv)) ^
                                 ") " ^ pp i ^ " =\n   " ^ 
                                 (String.concatWith "\n | " (map pp b)) ^ "]"
     | pp (ConBind (i,NONE)) = "ConBind [" ^ pp i ^ "]"
     | pp (ConBind (i,SOME t)) = "ConBind [" ^ pp i ^ " of " ^ pp t ^ "]"
     | pp (StructDec l) = "Structure [" ^
                  (String.concatWith "\nand " (map pp l)) ^ "]"
     | pp (StructBind (i,b)) = "StructBind ["^ pp i ^" = "^ pp b ^ "]" 
     | pp (Structure l) = "Struct [\n   " ^
                  (String.concatWith "\n   " (map pp l)) ^ "]"

   fun prettyPrint p = pp p

end

(* Generated from kuru.kpg *)

structure KuruParser =
struct
(* Kuru grammar description file *)
(* Gian Perrone, 26 September 2010 *)


structure P = ParseTree


val srcFileLineNum = ref 1


type res_char = string option
type res_literal = string option
type res_sp = string option
type res__ = string option
type res_comment = string option
type res_commentc = string option
type res_num = string option
type res_numdig = string option
type res_int = P.pt option
type res_real = P.pt option
type res_negat = bool option
type res_charconst = P.pt option
type res_string = P.pt option
type res_stringcont = string option
type res_con = P.pt option
type res_reserved = string option
type res_nonid = string option
type res_keyword = string option
type res_id = P.pt option
type res_idrest = string option
type res_var = P.pt option
type res_longid = P.pt option
type res_exp = P.pt option
type res_expel = P.pt option
type res_explist = P.pt list option
type res_expseq = P.pt list option
type res_match = P.pt list option
type res_pat = P.pt option
type res_patel = P.pt option
type res_patlist = P.pt list option
type res_patapp = P.pt list option
type res_typ = P.pt option
type res_typel = P.pt option
type res_tyannopt = P.pt option option
type res_declist = P.pt list option
type res_dec = P.pt option
type res_valbind = P.pt list option
type res_tyvarlist = P.pt list option
type res_tyvars = P.pt list option
type res_typebind = P.pt list option
type res_funbind = P.pt list list option
type res_funmatch = P.pt list option
type res_databind = P.pt list option
type res_conbind = P.pt list option
type res_strbind = P.pt list option
type res_str = P.pt option
type res_programel = P.pt option
type res_program = P.pt list option

val cache_program = ref [] : (int * (int * res_program)) list ref
val cache_programel = ref [] : (int * (int * res_programel)) list ref
val cache_str = ref [] : (int * (int * res_str)) list ref
val cache_strbind = ref [] : (int * (int * res_strbind)) list ref
val cache_conbind = ref [] : (int * (int * res_conbind)) list ref
val cache_databind = ref [] : (int * (int * res_databind)) list ref
val cache_funmatch = ref [] : (int * (int * res_funmatch)) list ref
val cache_funbind = ref [] : (int * (int * res_funbind)) list ref
val cache_typebind = ref [] : (int * (int * res_typebind)) list ref
val cache_tyvars = ref [] : (int * (int * res_tyvars)) list ref
val cache_tyvarlist = ref [] : (int * (int * res_tyvarlist)) list ref
val cache_valbind = ref [] : (int * (int * res_valbind)) list ref
val cache_dec = ref [] : (int * (int * res_dec)) list ref
val cache_declist = ref [] : (int * (int * res_declist)) list ref
val cache_tyannopt = ref [] : (int * (int * res_tyannopt)) list ref
val cache_typel = ref [] : (int * (int * res_typel)) list ref
val cache_typ = ref [] : (int * (int * res_typ)) list ref
val cache_patapp = ref [] : (int * (int * res_patapp)) list ref
val cache_patlist = ref [] : (int * (int * res_patlist)) list ref
val cache_patel = ref [] : (int * (int * res_patel)) list ref
val cache_pat = ref [] : (int * (int * res_pat)) list ref
val cache_match = ref [] : (int * (int * res_match)) list ref
val cache_expseq = ref [] : (int * (int * res_expseq)) list ref
val cache_explist = ref [] : (int * (int * res_explist)) list ref
val cache_expel = ref [] : (int * (int * res_expel)) list ref
val cache_exp = ref [] : (int * (int * res_exp)) list ref
val cache_longid = ref [] : (int * (int * res_longid)) list ref
val cache_var = ref [] : (int * (int * res_var)) list ref
val cache_idrest = ref [] : (int * (int * res_idrest)) list ref
val cache_id = ref [] : (int * (int * res_id)) list ref
val cache_keyword = ref [] : (int * (int * res_keyword)) list ref
val cache_nonid = ref [] : (int * (int * res_nonid)) list ref
val cache_reserved = ref [] : (int * (int * res_reserved)) list ref
val cache_con = ref [] : (int * (int * res_con)) list ref
val cache_stringcont = ref [] : (int * (int * res_stringcont)) list ref
val cache_string = ref [] : (int * (int * res_string)) list ref
val cache_charconst = ref [] : (int * (int * res_charconst)) list ref
val cache_negat = ref [] : (int * (int * res_negat)) list ref
val cache_real = ref [] : (int * (int * res_real)) list ref
val cache_int = ref [] : (int * (int * res_int)) list ref
val cache_num = ref [] : (int * (int * res_num)) list ref
val cache_commentc = ref [] : (int * (int * res_commentc)) list ref
val cache_comment = ref [] : (int * (int * res_comment)) list ref
val cache__ = ref [] : (int * (int * res__)) list ref
val cache_sp = ref [] : (int * (int * res_sp)) list ref

fun $ f = valOf f

val errorPos = ref 0
fun setErrPos c = if !errorPos < c then errorPos := c else ()
val debugVerbose = ref false
fun debug_print s = if (!debugVerbose) then print s else ()
fun notNone (NONE : 'a option) = false | notNone (SOME _) = true
fun cachefind c p =
  (fn NONE => NONE | SOME (k,v) => SOME v)
   (List.find (fn (p',v) => p = p') (!c))
fun cacheupd c p v =  (if length (!c) > 25 then (c := []) else (); c := (p,v) :: (!c)) fun error () = !errorPos
fun kupeg_start s = valOf (parse_program (s,ref 0))

and parse_char(input, pos) = 
  if (!pos >= size input) then NONE else
  (pos := !pos + 1; setErrPos (!pos); SOME (String.str (String.sub(input,(!pos - 1)))))
  handle Subscript => NONE
and parse_alpha (input, pos) =
  if (!pos >= size input) then NONE else
  let
     val c = String.sub(input,(!pos))
  in
     if Char.isAlpha c then SOME (pos := 1 + !pos; String.str c) else NONE
  end
and parse_digit (input, pos) =
  if (!pos >= size input) then NONE else
  let
     val c = String.sub(input,(!pos))
  in
     if Char.isDigit c then SOME (pos := 1 + !pos; String.str c) else NONE
  end
and literal(input, pos, str) = 
 (setErrPos (!pos);
  (if (String.substring(input, !pos, size str) = str) then
  (pos := !pos + size str; SOME  (str))
  else NONE) handle Subscript => NONE)

and parse_sp (input,pos) : res_sp =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_sp\n"
      val inppos = !pos
      val cacheLk = cachefind cache_sp inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 = literal(input,pos," ")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"\n")
   in
      if notNone s1' then SOME (((srcFileLineNum := !srcFileLineNum + 1);"\n"))   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; literal(input,pos,"\t"))
   end
)
   end
)
            val _ = cacheupd cache_sp inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse__ (input,pos) : res__ =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse__\n"
      val inppos = !pos
      val cacheLk = cachefind cache__ inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse_sp(input,pos)
   in
      if notNone s1' then parse__(input,pos)   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse_comment(input,pos)
   in
      if notNone s1' then parse__(input,pos)   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME "")
   end
)
   end
)
            val _ = cacheupd cache__ inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_comment (input,pos) : res_comment =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_comment\n"
      val inppos = !pos
      val cacheLk = cachefind cache_comment inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val s1' = literal(input,pos,"(*")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse_commentc(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"*)")
   in
      if notNone s1' then SOME ((""))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
)
            val _ = cacheupd cache_comment inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_commentc (input,pos) : res_commentc =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_commentc\n"
      val inppos = !pos
      val cacheLk = cachefind cache_commentc inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = literal(input,pos,"*)")
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse_char(input,pos)
   in
      if notNone s1' then parse_commentc(input,pos)   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME "")
   end
)
            val _ = cacheupd cache_commentc inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_num (input,pos) : res_num =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_num\n"
      val inppos = !pos
      val cacheLk = cachefind cache_num inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val d = parse_digit(input,pos)
   in
      if notNone d then (   let
      val prestate = !pos
      val n =    let
      fun fx () = let
         val prestate = !pos
         val t = (parse_digit(input,pos))
      in
        if notNone t then (valOf t) :: fx () else (pos := prestate; [])
      end
   in
      SOME (fx())
   end

   in
      if notNone n then (SOME ((String.concat($d :: $n)))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
)
            val _ = cacheupd cache_num inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_int (input,pos) : res_int =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_int\n"
      val inppos = !pos
      val cacheLk = cachefind cache_int inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val n = parse_negat(input,pos)
   in
      if notNone n then (   let
      val prestate = !pos
      val d = parse_num(input,pos)
   in
      if notNone d then (SOME ((P.Int ($n,$d)))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
)
            val _ = cacheupd cache_int inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_real (input,pos) : res_real =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_real\n"
      val inppos = !pos
      val cacheLk = cachefind cache_real inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val n1 = parse_negat(input,pos)
   in
      if notNone n1 then (   let
      val prestate = !pos
      val d1 = parse_num(input,pos)
   in
      if notNone d1 then (   let
      val prestate = !pos
      val s1' = literal(input,pos,".")
   in
      if notNone s1' then    let
      val prestate = !pos
      val d2 = parse_num(input,pos)
   in
      if notNone d2 then (   let
      val prestate = !pos
      val s1' = literal(input,pos,"e")
   in
      if notNone s1' then    let
      val prestate = !pos
      val n2 = parse_negat(input,pos)
   in
      if notNone n2 then (   let
      val prestate = !pos
      val d3 = parse_num(input,pos)
   in
      if notNone d3 then (SOME ((P.Real ($n1,$d1,$d2,SOME ($n2,$d3))))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val n1 = parse_negat(input,pos)
   in
      if notNone n1 then (   let
      val prestate = !pos
      val d1 = parse_num(input,pos)
   in
      if notNone d1 then (   let
      val prestate = !pos
      val s1' = literal(input,pos,".")
   in
      if notNone s1' then    let
      val prestate = !pos
      val d2 = parse_num(input,pos)
   in
      if notNone d2 then (SOME ((P.Real ($n1,$d1,$d2,NONE)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
)
   end
)
            val _ = cacheupd cache_real inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_negat (input,pos) : res_negat =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_negat\n"
      val inppos = !pos
      val cacheLk = cachefind cache_negat inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"~")
   in
      if notNone s1' then SOME ((true))   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME ((false)))
   end
)
            val _ = cacheupd cache_negat inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_charconst (input,pos) : res_charconst =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_charconst\n"
      val inppos = !pos
      val cacheLk = cachefind cache_charconst inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val s1' = literal(input,pos,"#")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"\"")
   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_char(input,pos)
   in
      if notNone c then (   let
      val prestate = !pos
      val s1' = literal(input,pos,"\"")
   in
      if notNone s1' then SOME ((P.Char ($c)))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
)
            val _ = cacheupd cache_charconst inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_string (input,pos) : res_string =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_string\n"
      val inppos = !pos
      val cacheLk = cachefind cache_string inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val s1' = literal(input,pos,"\"")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s = parse_stringcont(input,pos)
   in
      if notNone s then (   let
      val prestate = !pos
      val s1' = literal(input,pos,"\"")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME ((P.String ($s)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
            val _ = cacheupd cache_string inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_stringcont (input,pos) : res_stringcont =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_stringcont\n"
      val inppos = !pos
      val cacheLk = cachefind cache_stringcont inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = literal(input,pos,"\\")
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = literal(input,pos,"\"")
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_char(input,pos)
   in
      if notNone c then (   let
      val prestate = !pos
      val s = parse_stringcont(input,pos)
   in
      if notNone s then (SOME (($c ^ $s))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val b = literal(input,pos,"\\")
   in
      if notNone b then (   let
      val prestate = !pos
      val c = parse_char(input,pos)
   in
      if notNone c then (   let
      val prestate = !pos
      val s = parse_stringcont(input,pos)
   in
      if notNone s then (SOME (($b ^ $c ^ $s))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME (("")))
   end
)
   end
)
            val _ = cacheupd cache_stringcont inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_con (input,pos) : res_con =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_con\n"
      val inppos = !pos
      val cacheLk = cachefind cache_con inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 = parse_real(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = parse_int(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = parse_charconst(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate; parse_string(input,pos))
   end
)
   end
)
   end
)
            val _ = cacheupd cache_con inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_reserved (input,pos) : res_reserved =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_reserved\n"
      val inppos = !pos
      val cacheLk = cachefind cache_reserved inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 = literal(input,pos,"!")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"%")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"&")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"$")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"+")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"-")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"/")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"<=")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,">=")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,":=")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"::")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"<")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,">")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"?")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"@")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"\\")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"`")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"^")
   in
      if notNone opt1 then opt1 else
      (pos := prestate; literal(input,pos,"*"))
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_reserved inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_nonid (input,pos) : res_nonid =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_nonid\n"
      val inppos = !pos
      val cacheLk = cachefind cache_nonid inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 = literal(input,pos,"(")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,")")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"[")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"]")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,".")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,";")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,",")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"|")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"~")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"#")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,":")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"=>")
   in
      if notNone opt1 then opt1 else
      (pos := prestate; literal(input,pos,"="))
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_nonid inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_keyword (input,pos) : res_keyword =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_keyword\n"
      val inppos = !pos
      val cacheLk = cachefind cache_keyword inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 = literal(input,pos,"val")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"let")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"if")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"then")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"else")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"datatype")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"type")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"fn")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"fun")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"and")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"end")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"in")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"structure")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"struct")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"of")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"case")
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = literal(input,pos,"handle")
   in
      if notNone opt1 then opt1 else
      (pos := prestate; literal(input,pos,"raise"))
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_keyword inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_id (input,pos) : res_id =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_id\n"
      val inppos = !pos
      val cacheLk = cachefind cache_id inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val r1 = parse_reserved(input,pos)
   in
      if notNone r1 then (   let
      val prestate = !pos
      val r2 =    let
      fun fx () = let
         val prestate = !pos
         val t = (parse_reserved(input,pos))
      in
        if notNone t then (valOf t) :: fx () else (pos := prestate; [])
      end
   in
      SOME (fx())
   end

   in
      if notNone r2 then (SOME ((P.Ident(String.concat($r1 :: $r2))))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val k = parse_keyword(input,pos)
   in
      if notNone k then (   let
      val prestate = !pos
      val i1 = parse_idrest(input,pos)
   in
      if notNone i1 then (   let
      val prestate = !pos
      val i2 =    let
      fun fx () = let
         val prestate = !pos
         val t = (parse_idrest(input,pos))
      in
        if notNone t then (valOf t) :: fx () else (pos := prestate; [])
      end
   in
      SOME (fx())
   end

   in
      if notNone i2 then (SOME ((P.Ident($k ^ $i1 ^String.concat($i2))))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = parse_keyword(input,pos)
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_alpha(input,pos)
   in
      if notNone l then (   let
      val prestate = !pos
      val r =    let
      fun fx () = let
         val prestate = !pos
         val t = (parse_idrest(input,pos))
      in
        if notNone t then (valOf t) :: fx () else (pos := prestate; [])
      end
   in
      SOME (fx())
   end

   in
      if notNone r then (SOME ((P.Ident(String.concat ($l :: $r))))) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_id inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_idrest (input,pos) : res_idrest =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_idrest\n"
      val inppos = !pos
      val cacheLk = cachefind cache_idrest inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = parse_sp(input,pos)
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = parse_nonid(input,pos)
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' =    let
      val prestate = !pos
      val t = parse_reserved(input,pos)
      val _ = if notNone t then (pos := prestate) else ()
   in
      (fn NONE => SOME "" | SOME _ => NONE) t
   end

   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_char(input,pos)
   in
      if notNone c then (SOME (($c))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
)
            val _ = cacheupd cache_idrest inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_var (input,pos) : res_var =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_var\n"
      val inppos = !pos
      val cacheLk = cachefind cache_var inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val s1' = literal(input,pos,"'")
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (SOME ((P.Var ($i)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
            val _ = cacheupd cache_var inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_longid (input,pos) : res_longid =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_longid\n"
      val inppos = !pos
      val cacheLk = cachefind cache_longid inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = literal(input,pos,".")
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_longid(input,pos)
   in
      if notNone l then (SOME ((P.LongId ($i,$l)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (SOME (($i))) else (pos := prestate;NONE)
   end
)
   end
)
            val _ = cacheupd cache_longid inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_exp (input,pos) : res_exp =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_exp\n"
      val inppos = !pos
      val cacheLk = cachefind cache_exp inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"raise")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (SOME ((P.Raise ($e)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_expel(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"andalso")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_exp(input,pos)
   in
      if notNone e2 then (SOME ((P.AndAlso($e1,$e2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_expel(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"orelse")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_exp(input,pos)
   in
      if notNone e2 then (SOME ((P.OrElse($e1,$e2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_expel(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"handle")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val m = parse_match(input,pos)
   in
      if notNone m then (SOME ((P.HandleExp($e,$m)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_expel(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_exp(input,pos)
   in
      if notNone e2 then (SOME ((P.Infix($e1,$i,$e2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_expel(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_exp(input,pos)
   in
      if notNone e2 then (SOME ((P.App($e1,$e2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_expel(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,":")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (SOME ((P.TyAnn ($e,$t)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_expel(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_exp(input,pos)
   in
      if notNone e2 then (SOME ((P.Infix($e1,P.Ident "=",$e2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_expel(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME (($e))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_exp inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_expel (input,pos) : res_expel =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_expel\n"
      val inppos = !pos
      val cacheLk = cachefind cache_expel inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"op")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_longid(input,pos)
   in
      if notNone l then (SOME ((P.OpExp ($l)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"let")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val d = parse_declist(input,pos)
   in
      if notNone d then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"in")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,";")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_expseq(input,pos)
   in
      if notNone e2 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"end")
   in
      if notNone s1' then SOME ((P.LetExp($d,P.SeqExp($e1 :: $e2))))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"let")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val d = parse_declist(input,pos)
   in
      if notNone d then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"in")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"end")
   in
      if notNone s1' then SOME ((P.LetExp($d,$e)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 = parse_con(input,pos)
   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,",")
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_explist(input,pos)
   in
      if notNone e2 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME ((P.TupleExp ($e1 :: $e2)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,";")
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_expseq(input,pos)
   in
      if notNone e2 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME ((P.SeqExp ($e1 :: $e2)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME (($e))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"()")
   in
      if notNone s1' then SOME ((P.UnitExp))   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"[")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,",")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_explist(input,pos)
   in
      if notNone e2 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"]")
   in
      if notNone s1' then SOME ((P.ListExp ($e1 :: $e2)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"[")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"]")
   in
      if notNone s1' then SOME ((P.ListExp [$e]))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"[")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"]")
   in
      if notNone s1' then SOME ((P.ListExp []))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"if")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"then")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_exp(input,pos)
   in
      if notNone e2 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"else")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e3 = parse_exp(input,pos)
   in
      if notNone e3 then (SOME ((P.IfExp($e1,$e2,$e3)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"while")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"do")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_exp(input,pos)
   in
      if notNone e2 then (SOME ((P.WhileExp($e1,$e2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"case")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"of")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val m = parse_match(input,pos)
   in
      if notNone m then (SOME ((P.CaseExp($e,$m)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"fn")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val m = parse_match(input,pos)
   in
      if notNone m then (SOME ((P.FnExp($m)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; parse_longid(input,pos))
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_expel inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_explist (input,pos) : res_explist =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_explist\n"
      val inppos = !pos
      val cacheLk = cachefind cache_explist inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,",")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_explist(input,pos)
   in
      if notNone e2 then (SOME (($e1 :: $e2))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (SOME ((([$e])))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_explist inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_expseq (input,pos) : res_expseq =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_expseq\n"
      val inppos = !pos
      val cacheLk = cachefind cache_expseq inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e1 = parse_exp(input,pos)
   in
      if notNone e1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,";")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e2 = parse_expseq(input,pos)
   in
      if notNone e2 then (SOME (($e1 :: $e2))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (SOME ((([$e])))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_expseq inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_match (input,pos) : res_match =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_match\n"
      val inppos = !pos
      val cacheLk = cachefind cache_match inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p = parse_pat(input,pos)
   in
      if notNone p then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=>")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"|")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val m = parse_match(input,pos)
   in
      if notNone m then (SOME (((P.Match($p,$e)) :: $m))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p = parse_pat(input,pos)
   in
      if notNone p then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=>")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (SOME (([P.Match($p,$e)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_match inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_pat (input,pos) : res_pat =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_pat\n"
      val inppos = !pos
      val cacheLk = cachefind cache_pat inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p1 = parse_patel(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"as")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p2 = parse_pat(input,pos)
   in
      if notNone p2 then (SOME ((P.AsPat($p1,$p2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p1 = parse_patel(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p2 = parse_pat(input,pos)
   in
      if notNone p2 then (SOME ((P.InfixPat($p1,$i,$p2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val i = parse_longid(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p = parse_pat(input,pos)
   in
      if notNone p then (SOME ((P.ConstrPat ($i,SOME ($p))))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p = parse_patel(input,pos)
   in
      if notNone p then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,":")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (SOME ((P.TyAnnPat ($p,$t)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then parse_patel(input,pos)   else (pos := prestate; NONE)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_pat inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_patel (input,pos) : res_patel =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_patel\n"
      val inppos = !pos
      val cacheLk = cachefind cache_patel inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"_")
   in
      if notNone s1' then SOME ((P.WildcardPat))   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p1 = parse_pat(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,",")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p2 = parse_patlist(input,pos)
   in
      if notNone p2 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME ((P.TuplePat ($p1 :: $p2)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p = parse_pat(input,pos)
   in
      if notNone p then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME (($p))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"()")
   in
      if notNone s1' then SOME ((P.UnitPat))   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"[")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p = parse_patlist(input,pos)
   in
      if notNone p then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"]")
   in
      if notNone s1' then SOME ((P.ListPat ($p)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"[")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"]")
   in
      if notNone s1' then SOME ((P.ListPat []))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val c = parse_con(input,pos)
   in
      if notNone c then (SOME ((P.ConPat ($c)))) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val i = parse_longid(input,pos)
   in
      if notNone i then (SOME ((P.IdPat ($i)))) else (pos := prestate;NONE)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_patel inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_patlist (input,pos) : res_patlist =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_patlist\n"
      val inppos = !pos
      val cacheLk = cachefind cache_patlist inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p1 = parse_pat(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,",")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p2 = parse_patlist(input,pos)
   in
      if notNone p2 then (SOME (($p1 :: $p2))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p1 = parse_pat(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then SOME (([$p1]))   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_patlist inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_patapp (input,pos) : res_patapp =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_patapp\n"
      val inppos = !pos
      val cacheLk = cachefind cache_patapp inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p1 = parse_pat(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p2 = parse_patlist(input,pos)
   in
      if notNone p2 then (SOME (($p1 :: $p2))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p1 = parse_pat(input,pos)
   in
      if notNone p1 then (SOME (([$p1]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_patapp inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_typ (input,pos) : res_typ =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_typ\n"
      val inppos = !pos
      val cacheLk = cachefind cache_typ inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t1 = parse_typel(input,pos)
   in
      if notNone t1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"->")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t2 = parse_typ(input,pos)
   in
      if notNone t2 then (SOME ((P.TyArrow ($t1,$t2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t1 = parse_typel(input,pos)
   in
      if notNone t1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"*")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t2 = parse_typ(input,pos)
   in
      if notNone t2 then (SOME ((P.TyPair ($t1,$t2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t1 = parse_typel(input,pos)
   in
      if notNone t1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t2 = parse_typ(input,pos)
   in
      if notNone t2 then (SOME ((P.TyCon ($t1,$t2)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t1 = parse_typel(input,pos)
   in
      if notNone t1 then (SOME (($t1))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_typ inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_typel (input,pos) : res_typel =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_typel\n"
      val inppos = !pos
      val cacheLk = cachefind cache_typel inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val v = parse_var(input,pos)
   in
      if notNone v then (SOME (($v))) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME (($t))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val i = parse_longid(input,pos)
   in
      if notNone i then (SOME ((P.TyName ($i)))) else (pos := prestate;NONE)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_typel inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_tyannopt (input,pos) : res_tyannopt =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_tyannopt\n"
      val inppos = !pos
      val cacheLk = cachefind cache_tyannopt inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,":")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (SOME ((SOME ($t)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME ((NONE)))
   end
)
            val _ = cacheupd cache_tyannopt inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_declist (input,pos) : res_declist =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_declist\n"
      val inppos = !pos
      val cacheLk = cachefind cache_declist inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      fun fx () = let
         val prestate = !pos
         val t = (parse_dec(input,pos))
      in
        if notNone t then (valOf t) :: fx () else (pos := prestate; [])
      end
   in
      SOME (fx())
   end
)
            val _ = cacheupd cache_declist inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_dec (input,pos) : res_dec =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_dec\n"
      val inppos = !pos
      val cacheLk = cachefind cache_dec inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"val")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val v = parse_valbind(input,pos)
   in
      if notNone v then (SOME ((P.ValDec($v)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"type")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val v = parse_typebind(input,pos)
   in
      if notNone v then (SOME ((P.TypeDec($v)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"datatype")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"datatype")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_longid(input,pos)
   in
      if notNone l then (SOME ((P.DatatypeAssign ($i,$l)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"datatype")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val d = parse_databind(input,pos)
   in
      if notNone d then (SOME ((P.DatatypeDec ($d)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"fun")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val f = parse_funbind(input,pos)
   in
      if notNone f then (SOME ((P.FunDec ($f)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"structure")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s = parse_strbind(input,pos)
   in
      if notNone s then (SOME ((P.StructDec ($s)))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_dec inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_valbind (input,pos) : res_valbind =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_valbind\n"
      val inppos = !pos
      val cacheLk = cachefind cache_valbind inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val p = parse_pat(input,pos)
   in
      if notNone p then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"and")
   in
      if notNone s1' then    let
      val prestate = !pos
      val v = parse_valbind(input,pos)
   in
      if notNone v then (SOME ((P.ValBind ($p,$e) :: $v))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val p = parse_pat(input,pos)
   in
      if notNone p then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (SOME (([P.ValBind ($p,$e)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
)
   end
)
            val _ = cacheupd cache_valbind inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_tyvarlist (input,pos) : res_tyvarlist =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_tyvarlist\n"
      val inppos = !pos
      val cacheLk = cachefind cache_tyvarlist inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val v = parse_var(input,pos)
   in
      if notNone v then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,",")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_tyvarlist(input,pos)
   in
      if notNone t then (SOME (($v :: $t))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val v = parse_var(input,pos)
   in
      if notNone v then (SOME (([$v]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_tyvarlist inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_tyvars (input,pos) : res_tyvars =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_tyvars\n"
      val inppos = !pos
      val cacheLk = cachefind cache_tyvars inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"(")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_tyvarlist(input,pos)
   in
      if notNone l then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,")")
   in
      if notNone s1' then SOME (($l))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val v = parse_var(input,pos)
   in
      if notNone v then (SOME (([$v]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; SOME (([])))
   end
)
   end
)
            val _ = cacheupd cache_tyvars inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_typebind (input,pos) : res_typebind =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_typebind\n"
      val inppos = !pos
      val cacheLk = cachefind cache_typebind inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val tv = parse_tyvars(input,pos)
   in
      if notNone tv then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"and")
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_typebind(input,pos)
   in
      if notNone l then (SOME ((P.TypeBind($tv,$i,$t) :: $l))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val tv = parse_tyvars(input,pos)
   in
      if notNone tv then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (SOME (([P.TypeBind($tv,$i,$t)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_typebind inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_funbind (input,pos) : res_funbind =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_funbind\n"
      val inppos = !pos
      val cacheLk = cachefind cache_funbind inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val f = parse_funmatch(input,pos)
   in
      if notNone f then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"and")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_funbind(input,pos)
   in
      if notNone l then (SOME (($f :: $l))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val f = parse_funmatch(input,pos)
   in
      if notNone f then (SOME (([$f]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_funbind inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_funmatch (input,pos) : res_funmatch =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_funmatch\n"
      val inppos = !pos
      val cacheLk = cachefind cache_funmatch inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"op")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_patapp(input,pos)
   in
      if notNone l then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_tyannopt(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"|")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val f = parse_funmatch(input,pos)
   in
      if notNone f then (SOME ((P.FunMatch (P.OpExp ($i),$l,$t,$e) :: $f))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = literal(input,pos,"op")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val l = parse_patapp(input,pos)
   in
      if notNone l then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_tyannopt(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (SOME (([P.FunMatch(P.OpExp ($i),$l,$t,$e)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val l = parse_patapp(input,pos)
   in
      if notNone l then (   let
      val prestate = !pos
      val t = parse_tyannopt(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"|")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val f = parse_funmatch(input,pos)
   in
      if notNone f then (SOME ((P.FunMatch ($i,$l,$t,$e) :: $f))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val l = parse_patapp(input,pos)
   in
      if notNone l then (   let
      val prestate = !pos
      val t = parse_tyannopt(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val e = parse_exp(input,pos)
   in
      if notNone e then (SOME (([P.FunMatch ($i,$l,$t,$e)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
) else (pos := prestate;NONE)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_funmatch inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_databind (input,pos) : res_databind =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_databind\n"
      val inppos = !pos
      val cacheLk = cachefind cache_databind inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val tv = parse_tyvars(input,pos)
   in
      if notNone tv then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_conbind(input,pos)
   in
      if notNone c then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"and")
   in
      if notNone s1' then    let
      val prestate = !pos
      val d = parse_databind(input,pos)
   in
      if notNone d then (SOME ((P.DataBind($tv,$i,$c) :: $d))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val tv = parse_tyvars(input,pos)
   in
      if notNone tv then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_conbind(input,pos)
   in
      if notNone c then (SOME (([P.DataBind($tv,$i,$c)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_databind inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_conbind (input,pos) : res_conbind =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_conbind\n"
      val inppos = !pos
      val cacheLk = cachefind cache_conbind inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"of")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"|")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_conbind(input,pos)
   in
      if notNone c then (SOME ((P.ConBind($i,SOME ($t)) :: $c))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"of")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val t = parse_typ(input,pos)
   in
      if notNone t then (SOME (([P.ConBind($i,SOME ($t))]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"|")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val c = parse_conbind(input,pos)
   in
      if notNone c then (SOME ((P.ConBind($i,NONE) :: $c))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (SOME (([P.ConBind($i,NONE)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_conbind inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_strbind (input,pos) : res_strbind =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_strbind\n"
      val inppos = !pos
      val cacheLk = cachefind cache_strbind inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s = parse_str(input,pos)
   in
      if notNone s then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"and")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val st = parse_strbind(input,pos)
   in
      if notNone st then (SOME ((P.StructBind($i,$s) :: $st))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val i = parse_id(input,pos)
   in
      if notNone i then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"=")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s = parse_str(input,pos)
   in
      if notNone s then (SOME (([P.StructBind($i,$s)]))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
)
   end
)
            val _ = cacheupd cache_strbind inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_str (input,pos) : res_str =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_str\n"
      val inppos = !pos
      val cacheLk = cachefind cache_str inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"struct")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val d = parse_declist(input,pos)
   in
      if notNone d then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,"end")
   in
      if notNone s1' then SOME ((P.Structure($d)))   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate; parse_longid(input,pos))
   end
)
            val _ = cacheupd cache_str inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_programel (input,pos) : res_programel =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_programel\n"
      val inppos = !pos
      val cacheLk = cachefind cache_programel inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then parse_dec(input,pos)   else (pos := prestate; NONE)
   end
)
            val _ = cacheupd cache_programel inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


and parse_program (input,pos) : res_program =
   let
      val _ = setErrPos (!pos)
      val _ = debug_print "parse_program\n"
      val inppos = !pos
      val cacheLk = cachefind cache_program inppos
      val cacheGen = if notNone cacheLk then
         let val (cPos,cVal) = valOf cacheLk
             val _ = pos := cPos
         in cVal end
         else let
            val cVal = (   let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val p1 = parse_programel(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = literal(input,pos,";")
   in
      if notNone s1' then    let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p2 = parse_program(input,pos)
   in
      if notNone p2 then (SOME (($p1 :: $p2))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val opt1 =    let
      val prestate = !pos
      val p1 = parse_programel(input,pos)
   in
      if notNone p1 then (   let
      val prestate = !pos
      val s1' = parse__(input,pos)
   in
      if notNone s1' then    let
      val prestate = !pos
      val p2 = parse_program(input,pos)
   in
      if notNone p2 then (SOME (($p1 :: $p2))) else (pos := prestate;NONE)
   end
   else (pos := prestate; NONE)
   end
) else (pos := prestate;NONE)
   end

   in
      if notNone opt1 then opt1 else
      (pos := prestate;    let
      val prestate = !pos
      val p = parse_programel(input,pos)
   in
      if notNone p then (SOME (([$p]))) else (pos := prestate;NONE)
   end
)
   end
)
   end
)
            val _ = cacheupd cache_program inppos (!pos,cVal)
              in cVal end
   in
      cacheGen
   end


end
structure Main =
struct
   val args = CommandLine.arguments ()

   fun parseArgs [] = []
	  | parseArgs ("-v"::t) = (KuruParser.debugVerbose := true; 
                              parseArgs t)
     | parseArgs (h::t) = h :: parseArgs t
      
   val args' = parseArgs args 

   val _ = if length args' < 1 then 
            raise Fail "Usage: kuruc [-v] filename.k" else ()
	
   val linepositions = ref [] : int list ref
   val p = ref 0

   fun readLines fp = 
      let
         val l = TextIO.inputLine fp
      in
         case l of NONE => ""
                 | SOME l' => (
                    linepositions := !linepositions @ [!p];
                    p := !p + size l';
                    l' ^ readLines fp)
      end


   val filename = hd args'

   val f = TextIO.openIn filename
   val buf = readLines f
   val _ = TextIO.closeIn f

   val p = KuruParser.kupeg_start buf handle Option => 
      raise Fail ("kuruc: error near " ^ 
                  (Error.resolveErrorPos(filename,
                                         !linepositions,
                                         KuruParser.error(),
                                         buf)))

   val _ = if KuruParser.error() + 20 < size buf then 
      print ("kuruc: maybe error near " ^ 
                  (Error.resolveErrorPos(filename,
                                         !linepositions,
                                         KuruParser.error(),
                                         buf)) ^ "\n") else ()


   val pretty = (String.concatWith "\n" (map ParseTree.prettyPrint p))

   fun main () = print (pretty ^ "\n")
end   
   
val _ = Main.main ()


