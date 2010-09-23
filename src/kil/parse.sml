structure KilParse =
struct
   structure A = Ast

   val line = ref 1
   val position = ref 0

   val s = ref [] : char list ref (* input string *)
   
   fun nline () = (line := !line + 1; position := 0)
   fun advance  n = position := !position + n
   fun sadvance s = position := !position + size s
   fun next () = if length (!s) = 0 then Char.chr 0 else 
      let
         val _ = print "NEXT\n"
         val h = hd (!s)
         val _ = advance 1
      in
         (s := tl (!s); h)
      end

   fun eof () = length (!s) = 0

   fun error s = raise (Fail 
                  ("Error near line " ^ Int.toString (!line) ^ ":" ^
                   Int.toString (!position) ^ ": " ^ s))


   fun matches tok = if length (!s) < size tok then false else 
                        String.isPrefix tok (String.implode (!s))

   fun consume tok =
      let
         val _ = print ("Consuming token: " ^ tok ^ "\n")
         fun c [] = []
           | c (h::t) = if (let val k = next (); val _ = print ("chr: " ^ String.str k ^ "\n") in k end) = h then c t else error ("Expected '" ^ tok ^ "', instead got: '" ^ (String.implode (!s)) ^ "'" )
         val _ = sadvance tok
      in
         c (String.explode tok)
      end

   fun ws' (#" "::t)  = (advance 1; ws' t)
     | ws' (#"\t"::t) = (advance 1; ws' t)
     | ws' (#"\n"::t) = (nline (); ws' t)
     | ws' l = l

   fun ws () = s := ws' (!s)


   fun wconsume tok =
      let 
         val _ = ws ()
         val _ = consume tok
         val _ = ws ()
      in
         ()
      end

   fun peek () = if length (!s) = 0 then Char.chr 0 else hd (!s)



   fun valid_id #"!" = true
     | valid_id #"&" = true
     | valid_id #"$" = true
     | valid_id #"#" = true
     | valid_id #"+" = true
     | valid_id #"-" = true
     | valid_id #"/" = true
     | valid_id #":" = true
     | valid_id #"<" = true
     | valid_id #">" = true
     | valid_id #"?" = true
     | valid_id #"@" = true
     | valid_id #"\\" = true
     | valid_id #"~" = true
     | valid_id #"`" = true
     | valid_id #"^" = true
     | valid_id #"|" = true
     | valid_id #"*" = true
     | valid_id _ = false


   fun is_digit () = Char.isDigit (peek()) 
   fun is_tyvar () = peek() = #"'" 
   fun is_tyvar_list () = is_tyvar () orelse matches "('" 
   fun is_id () =
      let
         val h = peek()
         val c1 = Char.isLower h
         val c2 = valid_id h
      in
         c1 orelse c2
      end
   fun is_con () = matches "#\"" orelse 
                   matches "~" orelse 
                   is_digit () orelse
                   matches "\"" 
   fun is_exp () = not (peek() = Char.chr 0) andalso (
                   is_con () orelse
                   matches "op" orelse
                   matches "(" orelse
                   matches "[" orelse
                   matches "#" orelse
                   matches "let" orelse
                   matches "if" orelse
                   matches "raise" orelse
                   matches "while" orelse
                   matches "case" orelse
                   matches "fn")

   (* Ast builders *)

   fun mk_val vb = (print "mk_val vb\n"; A.ValDec vb)
   fun mk_datatype d = (print "mk_datatype d\n"; A.DatatypeBind d)
   fun mk_fun f = (print "mk_fun f\n"; A.FunBind f)
   fun mk_open l = (print "mk_open l\n"; A.Open l)
   fun mk_nonfix l = (print "mk_nonfix l\n"; A.Nonfix l)
   fun mk_infix l = (print "mk_infix l\n"; A.Infix l)
   fun mk_infixr l = (print "mk_infixr l\n"; A.Infixr l)
   fun mk_excep e = (print "mk_excep e\n"; A.ExceptionBind e)
   fun mk_local (a,b) = (print "mk_local l\n"; A.LocalDec (a,b))
   fun mk_rec l = (print "mk_rec l\n"; A.ValRec l)
   fun mk_funbind l = (print "mk_funbind l\n"; l)
   fun mk_funmatch l = (print "mk_funmatch l\n"; A.FunMatch l)
   fun mk_typedecl l = (print "mk_typebind l\n"; A.TypeDec l)
   fun mk_typebind l = (print "mk_typebind l\n"; A.TypeBind l)
   fun mk_conbind l = (print "mk_conbind l\n"; A.ConBind l)
   fun mk_exnbind l = (print "mk_exnbind l\n"; A.ExnBind l)
   fun mk_pat l = (print "mk_pat l\n"; A.Pat l)
   fun mk_pat_wildcard x = (print "mk_pat_wildcard x\n"; A.WildcardPat)
   fun mk_infix_pat l = (print "mk_infix_pat l\n"; A.InfixPat l)
   fun mk_as_pat l = (print "mk_as_pat l\n"; A.AsPat l)
   fun mk_id_pat i = (print "mk_id_pat i\n"; A.IdPat i)
   fun mk_op_pat l = (print "mk_op_pat l\n"; A.OpPat l)
   fun mk_tup_pat t = (print "mk_tup_pat t\n"; A.TuplePat t)
   fun mk_list_pat t = (print "mk_list_pat t\n"; A.ListPat t)
   fun mk_const_pat c = (print "mk_const_pat c\n"; A.ConstPat c)
   fun mk_typ_pat c = (print "mk_typ_pat c\n"; A.TypePat c)
   fun mk_fn_typ x = (print "mk_fn_typ x\n"; A.FnTyp x)
   fun mk_pair_typ x = (print "mk_pair_typ x\n"; A.PairTyp x)
   fun mk_cons_typ x = (print "mk_cons_typ x\n"; A.ConsTyp x)
   fun mk_raise_exp x = (print "mk_raise_exp x\n"; A.RaiseExp x)
   fun mk_andalso_exp x = (print "mk_andalso_exp x\n"; A.AndAlsoExp x)
   fun mk_orelse_exp x = (print "mk_orelse_exp x\n"; A.OrElseExp x)
   fun mk_infix_exp x = (print "mk_infix_exp x\n"; A.InfixExp x)
   fun mk_app_exp x = (print "mk_app_exp x\n"; A.AppExp x)
   fun mk_con_exp x = (print "mk_con_exp x\n"; A.ConstantExp x)
   fun mk_tup_exp x = (print "mk_tup_exp x\n"; A.TupleExp x)
   fun mk_seq_exp x = (print "mk_seq_exp x\n"; A.SeqExp x)
   fun mk_list_exp x = (print "mk_list_exp x\n"; A.ListExp x)
   fun mk_let_exp x = (print "mk_let_exp x\n"; A.LetExp x)
   fun mk_if_exp x = (print "mk_if_exp x\n"; A.IfExp x)
   fun mk_while_exp x = (print "mk_while_exp x\n"; A.WhileExp x)
   fun mk_case_exp x = (print "mk_case_exp x\n"; A.CaseExp x)
   fun mk_fn_exp x = (print "mk_fn_exp x\n"; A.FnExp x)
   fun mk_lab_exp x = (print "mk_lab_exp x\n"; A.LabelExp x)
   fun mk_handle_exp x = (print "mk_handle_exp x\n"; A.Null)
   fun mk_match x = (print "mk_match x\n"; A.Match x)
   fun mk_id x = (print "mk_id x\n"; A.Identifier x)
   fun mk_tyclass x = (print "mk_tyclass x\n"; A.Null)
   fun mk_tyvar x = (print "mk_tyvar x\n"; A.TyVar x)
   fun mk_int x = (print "mk_int x\n"; A.Int x)
   fun mk_char x = (print "mk_char x\n"; A.Null)
   fun mk_num_lab x = (print "mk_num_lab x\n"; A.Null)
   fun mk_id_lab x = (print "mk_id_lab x\n"; A.Null)
   fun mk_type x = (print "mk_type x\n"; A.Type x)
   fun mk_id_list x = (print "mk_id_list x\n"; x) 
   fun mk_valbind x = (print "mk_valbind x\n"; A.ValBind x)
   fun mk_op x = (print "mk_op x\n"; A.OpExp x )
   fun mk_databind x = (print "mk_databind x\n"; A.Null)

   fun id () =
      let
         val h = next()

         val _ = if not (Char.isLower h) andalso not (valid_id h) then 
            error "invalid identifier" else ()

         fun f [] = []
           | f (h::t) = if Char.isAlphaNum h orelse h = #"'" then 
                           h :: f t else []

         val vn = h :: f (!s)
         val vn' = String.implode vn

         val _ = sadvance vn'

         val _ = print ("id: pre-drop: " ^ (String.implode (!s)) ^ "\n")

         val _ = s := List.drop (!s,size vn'-1)
         val _ = print ("id: post-drop: " ^ (String.implode (!s)) ^ "\n")
      in
         mk_id vn'
      end

   fun id_list () =
      let
         val i = id ()

         fun f () = (ws(); if is_id () then id() :: id_list() else [])
      in
         mk_id_list (i :: f())
      end

   fun num () =
      let
         val _ = ws ()

         val _ = print "num\n"

         fun f [] = []
           | f (h::t) = if Char.isDigit h then h :: f t else []

         val vn = String.implode (f (!s))
         val _ = s := List.drop (!s,size vn)
         val vn' = valOf (Int.fromString vn)
      in
         vn'
      end

   fun con_string () = error "Strings aren't implemented"

   fun hex () = raise Fail "Hex literals not implemented"

   fun bin () = raise Fail "Binary literals not implemented"

   fun con_int () =
      let
         val neg = if matches "~" then (consume "~"; true) else false
         val n = if matches "0x" then hex () else
                 if matches "0b" then bin () else num ()
      in
         mk_int (neg,n)
      end

   fun con_char () =
      let
         val _ = consume "#\""
         val c = next ()
         val _ = consume "\""
      in
         mk_char c
      end

   fun con () =
      let
         val _ = ws ()
      in
         if matches "\"" then con_string() else
         if matches "~" orelse is_digit () then con_int() else
         if matches "#\"" then con_char() else
         error "Invalid constant value"
      end

   fun lab () =
      let
         val _ = ws ()
      in
         if is_id () then mk_id_lab (id()) else mk_num_lab (num())
      end

   fun longid () =
      let
         val i = id()
      in
         if matches "." then (consume "."; i :: longid()) else [i]
      end

   fun tyvar () =
      let
         val _ = ws ()
      in
         if matches "''" then (consume "''"; mk_tyclass (id())) else
         if matches "'" then (consume "'"; mk_tyvar (id())) else
         error "invalid tyvar"
      end

   fun tyvar_list_opt () = NONE (* FIXME *)

   fun match () =
      let
         val _ = ws ()
         val p = pat ()
         val _ = wconsume "=>"
         val e = exp ()
         val _ = ws ()
      in
         if matches "|" then (consume "|"; mk_match (p,e) :: match ()) else
            [mk_match (p,e)]
      end

   and exp_seq () =
      let
         val e = exp ()
         val _ = ws ()
      in
         if matches ";" then (consume ";"; e :: exp_seq ()) else [e]
      end

   and exp_list () =
      let
         val e = exp ()
         val _ = ws ()
      in
         if matches "," then (consume ","; e :: exp_list ()) else [e]
      end  

   and exp () =
      let
         val _ = print ("exp: " ^ String.implode (!s) ^ "\n")
         val _ = ws ()
      in
         if eof() then A.Null else
         if matches "raise" then (consume "raise"; mk_raise_exp (exp()))
         else let
            val e = exp_el ()
            val _ = ws ()
         in
            if matches "andalso" then 
               (consume "andalso"; mk_andalso_exp (e,exp())) else
            if matches "orelse" then 
               (consume "orelse"; mk_orelse_exp (e,exp())) else
            if is_id() then mk_infix_exp (e,id(),exp()) else
            if is_exp() then mk_app_exp (e,exp()) else e
         end
      end
   and exp_el () =
      let
         val _ = print ("exp_el: " ^ String.implode (!s) ^ "\n")
         val _ = ws ()
         
         fun e_par () =
            let
               val _ = wconsume "("
               val e = exp ()
               val _ = ws ()
            in
               if matches ")" then e else
               if matches "," then 
                  let
                     val el = exp_list ()
                     val _ = wconsume ")"
                  in mk_tup_exp ( e :: el ) end else
               if matches ";" then
                  let
                     val es = exp_seq ()
                     val _ = wconsume ")"
                  in mk_seq_exp (e :: es) end
               else error "invalid parenthesised expession"
            end

         fun e_sq () =
            let
               val _ = wconsume "["
               val e = exp ()
               val _ = ws ()
            in
               if matches "]" then mk_list_exp [e] else
               if matches "," then 
                  let
                     val el = exp_list ()
                     val _ = wconsume "]"
                  in mk_list_exp ( e :: el ) end 
               else error "invalid list expession"
            end

         fun e_let () =
            let
               val d = decl_list ()
               val _ = wconsume "in"
               val e = exp ()
               val _ = ws ()
            in
               if matches ";" then
                  let
                     val _ = wconsume ";"
                     val es = exp_seq ()
                     val _ = wconsume "end"
                  in
                     mk_let_exp(d,mk_seq_exp (e :: es))
                  end else
                  let
                     val _ = wconsume "end"
                  in
                     mk_let_exp(d,e)
                  end
            end

         fun e_if () =
            let
               val _ = wconsume "if"
               val e1 = exp ()
               val _ = wconsume "then"
               val e2 = exp ()
               val _ = wconsume "else"
               val e3 = exp ()
            in
               mk_if_exp (e1, e2, e3)
            end

         fun e_while () =
            let
               val _ = wconsume "while"
               val e1 = exp ()
               val _ = wconsume "do"
               val e2 = exp ()
            in
               mk_while_exp (e1,e2)
            end

         fun e_case () =
            let
               val _ = wconsume "case"
               val e1 = exp ()
               val _ = wconsume "of"
               val m = match ()
            in
               mk_case_exp (e1,m)
            end

         fun e_fn () =
            let
               val _ = wconsume "fn"
               val m = match ()
            in
               mk_fn_exp m
            end
      in
         if eof() then A.Null else
         if is_con() then mk_con_exp (con ()) else
         if matches "op" then (consume "op"; mk_op (longid())) else
         if matches "(" then e_par () else
         if matches "#" then mk_lab_exp (lab ()) else
         if matches "[" then e_sq () else
         if matches "let" then e_let() else
         if matches "if" then e_if() else
         if matches "while" then e_while() else
         if matches "case" then e_case () else
         if matches "fn" then e_fn() else
         if is_id() then (id()) else
         let
            (* FIXME: id vs id handle _ not handled *)
            val e = exp ()
            val _ = wconsume "handle"
            val m = match ()
         in
            mk_handle_exp (e,m)
         end
      end

   and typ () =
      let
         val t = typ_el ()
         val _ = ws ()
      in
         if matches "->" then (consume "->"; mk_fn_typ (t,typ ())) else
         if matches "*" then (consume "*"; mk_pair_typ (t,typ ())) else
         if is_tyvar () orelse matches "(" then mk_cons_typ (t,typ()) else mk_type t
      end
   and typ_el () =
      let
         val _ = ws ()
      in
         if matches "(" then
            let
               val _ = wconsume "("
               val t = typ ()
               val _ = wconsume ")"
            in
               t
            end
         else tyvar ()
      end

   and pat () =
      let
         val p = pat_el ()
         val _ = ws ()
      in
         if matches "as" then (wconsume "as"; mk_as_pat (p,pat ())) else
         if is_id () then mk_infix_pat (p,id (),pat()) else mk_pat p
      end

   and pat_el () =
      let
         val _ = ws ()

         fun p_op () =
            let
               val _ = wconsume "op"
            in
               mk_op_pat (id ()) (* FIXME *)
            end

         fun p_par () =
            let
               val _ = wconsume "("
               val p = pat ()
               val _ = ws ()
            in
               if matches "," then 
                  let
                     val _ = consume ","
                     val p' = mk_tup_pat (p :: pattern_list ())
                     val _ = wconsume "]"
                  in p' end
               else (wconsume ")"; p)
            end

         fun p_sq () =
            let
               val _ = wconsume "["
               val p = pat ()
               val _ = ws ()
            in
               if matches "," then 
                  let
                     val _ = consume ","
                     val p' = mk_list_pat (p :: pattern_list ())
                  in (wconsume "]"; p') end
               else (consume "]"; mk_list_pat [p])
            end
      in
         if matches "_" then mk_pat_wildcard () else
         if matches "op" then p_op () else
         if matches "(" then p_par () else
         if matches "[" then p_sq () else
         if is_id () then mk_id_pat (id ()) else
         if is_con () then mk_const_pat (con ()) else
         let
            val p = pat ()
            val _ = wconsume (":")
            val t = typ ()
         in
            mk_typ_pat (p,t)
         end
      end

   and exnbind () =
      let
         val i = id ()
         val _ = ws ()

         val t = if matches "of" then (wconsume "of"; SOME (typ ()))
                                 else NONE

         val _ = ws ()
      in
         if matches "and" then (consume "and"; mk_exnbind (i,t) :: exnbind ())
         else [mk_exnbind (i,t)]
      end

   and conbind () =
      let
         val i = id ()
         val _ = ws ()

         val t = if matches "of" then (wconsume "of"; SOME (typ ()))
                                 else NONE

         val _ = ws ()
      in
         if matches "|" then (wconsume "|"; mk_conbind (i,t) :: conbind ())
         else [mk_conbind (i,t)]
      end

   and databind () =
      let
         val tv = tyvar_list_opt ()
         val i = id ()
         val _ = wconsume "="
         val cb = conbind ()
         val _ = ws ()
      in
         if matches "and" then mk_databind (tv,i,cb) :: databind ()
         else [mk_databind (tv,i,cb)]
      end

   and typebind () =
      let
         val _ = print "typebind\n" 
         val tv = tyvar_list_opt ()
         val i = id ()
         val _ = wconsume "="
         val t = typ ()
         val _ = ws ()
      in
         if matches "and" then mk_typebind (tv,i,t) :: typebind ()
         else [mk_typebind (tv,i,t)]
      end

   and pattern_list () = [pat ()] (* FIXME *)

   and funmatch () =
      let
         val _ = ws ()

         fun fmop () =
            let
               val _ = wconsume "op"
               val i = mk_op ([id ()])
               val pl = pattern_list ()
               val t = tyann_opt ()
               val _ = wconsume "="
               val e = exp ()
               val _ = ws ()
            in
               if matches "|" then 
                  (consume "|"; mk_funmatch (i,pl,t,e) :: funmatch ())
               else [mk_funmatch (i,pl,t,e)]
            end

         fun fminfix () =
            let
               val _ = wconsume "("
               val p1 = pat ()
               val i = mk_op ([id ()])
               val p2 = pat ()
               val _ = wconsume ")"
               val pl = pattern_list ()
               val t = tyann_opt ()
               val _ = wconsume "="
               val e = exp ()
               val _ = ws ()
            in
               if matches "|" then 
                  (consume "|"; mk_funmatch (i,pl,t,e) :: funmatch ())
               else [mk_funmatch (i,pl,t,e)]
               (* FIXME: This is actually broken *)
            end

         fun fmnf () =
            let
               val i = id ()
               val pl = pattern_list ()
               val t = tyann_opt ()
               val _ = wconsume "="
               val e = exp ()
               val _ = ws ()
            in
               if matches "|" then 
                  (consume "|"; mk_funmatch (i,pl,t,e) :: funmatch ())
               else [mk_funmatch (i,pl,t,e)]
            end
      in
         if matches "op" then fmop () else
         if matches "("  then fminfix () else
         fmnf ()
         (* FIXME: pattern id pattern ... missing *)
      end

   and funbind () =
      let
         val _ = print "funbind\n" 
         val _ = ws ()
         val fm = funmatch ()
         val _ = ws ()
      in
         if matches "and" then (consume "and";
            (mk_funbind fm :: funbind ()))
         else [mk_funbind fm]
      end

   and valbind () =
      let
         val _ = print "valbind\n" 
         val _ = ws ()
      in
         if matches "rec" then (consume "rec"; [mk_rec (valbind ())]) else
         let
            val p = pat ()
            val _ = wconsume "="
            val e = exp ()
            val _ = ws ()
         in
            if matches "and" then 
               (consume "and"; (mk_valbind (p,e)) :: valbind ())
            else [mk_valbind (p,e)]
         end
      end

   and tyann_opt () = 
      let
         val _ = print "tyann_opt\n"
         val _ = ws ()
      in
         if matches ":" then (consume ":"; SOME (mk_type (typ ()))) else NONE
      end

   (* FIXME: missing withtype and datatype assignment *)
   and decl () =
      let
         val _ = print "decl\n"
         val _ = ws () in
      if matches "val"        then (consume "val"; mk_val (valbind())) else
      if matches "type"       then (consume "type"; mk_typedecl (typebind())) else
      if matches "datatype"   then (consume "datatype"; 
                                             mk_datatype (databind())) else
      if matches "fun"        then (consume "fun"; mk_fun (funbind())) else
      if matches "open"       then (consume "open"; mk_open(longid())) else
      if matches "nonfix"     then (consume "nonfix"; mk_nonfix(id_list())) 
      else if matches "infixr"     then (consume "infixr"; ws();
                  if is_digit () then mk_infixr(SOME (num()),id_list())
                                 else mk_infixr(NONE,id_list())) else
      if matches "infix"      then (wconsume "infix"; 
                  if is_digit () then mk_infix(SOME (num()),id_list())
                                 else mk_infix(NONE,id_list())) else
      if matches "exception"  then (consume "exception"; mk_excep(exnbind()))
      else if matches "local" then 
         let
            val _ = consume "local"
            val dec1 = decl_list ()
            val _ = wconsume "in"
            val dec2 = decl_list ()
            val _ = wconsume "end"
         in
            mk_local (dec1, dec2)
         end
      else error "Expected declaration"
   end

   and decl_list () =
      let
         val _ = ws ()
         val d = decl ()
         val _ = ws ()
      in
         if matches ";" then (wconsume ";"; d :: decl_list()) else [d]
      end (* FIXME: Requires semicolons always *)

   fun program_el () = 
      (* FIXME: Add structure/sig/functor rules *)
      decl ()

   fun program () =
      let
         val _ = ws ()
         val pl = program_el ()
         val _ = ws ()
      in
         if eof () then [pl]
         else if matches ";" then (consume ";"; pl :: program ())
         else pl :: program ()
      end
           

   fun parse source =
      let
         val _ = s := String.explode source
      in
         program ()
      end
end
