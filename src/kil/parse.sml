structure KilLex =
struct
   val line = ref 1
   val position = ref 0

   val s = ref [] : char list ref (* input string *)
   
   fun nline () = (line := !line + 1; position := 0)
   fun advance  n = position := !position + n
   fun sadvance s = position := !position + size s
   fun next () = 
      let
         val h = hd (!s)
         val _ = advance 1
      in
         (s := tl (!s); h)
      end

   fun eof () = length (!s) = 0

   fun matches tok =
      let
         fun c [] = true
           | c (h::t) = if next () = h then c t else false 
      in
         c (String.explode tok)
      end

   fun consume tok =
      let
         fun c [] = []
           | c (h::t) = if next () = h then c t else error ("Expected " ^ tok)
         val _ = sadvance tok
      in
         c (String.explode tok)
      end

   fun wconsume tok =
      let 
         val _ = ws ()
         val _ = consume tok
         val _ = ws ()
      in
         ()
      end

   fun peek () = hd (!s)

   fun error s = raise (Fail 
                  ("Error near line " ^ Int.toString (!line) ^ ":" ^^
                   Int.toString (!position) ^ ": " ^ s))

   fun ws (#" "::t)  = (advance 1; ws t)
     | ws (#"\t"::t) = (advance 1; ws t)
     | ws (#"\n"::t) = (nline (); ws t)
     | ws l = l

   fun is_digit () = false (* FIXME *)
   fun is_tyvar_list () = false (* FIXME *)

   (* Ast builders *)

   datatype ast = Null

   fun mk_val vb = Null
   fun mk_datatype d = Null
   fun mk_fun f = Null
   fun mk_open l = Null
   fun mk_nonfix l = Null
   fun mk_infix l = Null
   fun mk_infixr l = Null
   fun mk_excep e = Null
   fun mk_local l = Null
   fun mk_rec l = Null
   fun mk_funbind l = Null
   fun mk_funmatch l = Null
   fun mk_typebind l = Null
   fun mk_conbind l = Null
   fun mk_exnbind l = Null
   

   fun exnbind () =
      let
         val i = id ()
         val _ = ws ()

         val t = if matches "of" then (wconsume "of"; SOME typ ())
                                 else NONE

         val _ = ws ()
      in
         if matches "and" then (consume "and"; mk_exnbind (i,t) :: exnbind ())
         else [mk_exnbind (i,t)]
      end

   fun conbind () =
      let
         val i = id ()
         val _ = ws ()

         val t = if matches "of" then (wconsume "of"; SOME typ ())
                                 else NONE

         val _ = ws ()
      in
         if matches "|" then (wconsume "|"; mk_conbind (i,t) :: conbind ())
         else [mk_conbind (i,t)]
      end

   fun databind () =
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

   fun typebind () =
      let
         val tv = tyvar_list_opt ()
         val i = id ()
         val _ = wconsume "="
         val t = typ ()
         val _ = ws ()
      in
         if match "and" then mk_typebind (tv,i,t) :: typebind ()
         else [mk_typebind (tv,i,t)]
      end

   fun funmatch () =
      let
         val _ = ws ()

         fun fmop () =
            let
               val _ = wconsume "op"
               val i = mk_op (id ())
               val pl = pattern_list ()
               val t = tyann_opt ()
               val _ = wconsume "="
               val e = expr ()
               val _ = ws ()
            in
               if matches "|" then 
                  (consume "|"; mk_funmatch (i,pl,t,e) :: funmatch)
               else [mk_funmatch (i,pl,t,e)]
            end

         fun fminfix () =
            let
               val _ = wconsume "("
               val p1 = pattern ()
               val i = mk_op (id ())
               val p2 = pattern ()
               val _ = wconsume ")"
               val pl = pattern_list ()
               val t = tyann_opt ()
               val _ = wconsume "="
               val e = expr ()
               val _ = ws ()
            in
               if matches "|" then 
                  (consume "|"; mk_funmatch (i,pl,t,e) :: funmatch)
               else [mk_funmatch (i,pl,t,e)]
               (* FIXME: This is actually broken *)
            end

         fun fmnf () =
            let
               val i = id ()
               val pl = pattern_list ()
               val t = tyann_opt ()
               val _ = wconsume "="
               val e = expr ()
               val _ = ws ()
            in
               if matches "|" then 
                  (consume "|"; mk_funmatch (i,pl,t,e) :: funmatch)
               else [mk_funmatch (i,pl,t,e)]
            end
      in
         if matches "op" then fmop () else
         if matches "("  then fminfix () else
         fmnf ()
         (* FIXME: pattern id pattern ... missing *)
      end

   fun funbind () =
      let
         val _ ws ()
         val fm = funmatch ()
         val _ = ws ()
      in
         if matches "and" then (consume "and";
            (mk_funbind fm :: funbind ()))
         else [mk_funbind fm]
      end

   fun valbind () =
      let
         val _ = ws ()
      in
         if matches "rec" then (consume "rec"; mk_rec (valbind ())) else
         let
            val p = pattern ()
            val _ = wconsume "="
            val e = expr ()
            val _ = ws ()
         in
            if matches "and" then 
               (consume "and"; (mk_valbind (p,e)) :: valbind ())
            else [mk_valbind (p,e)]
         end
      end

   (* FIXME: missing withtype and datatype assignment *)
   fun decl () =
      let val _ = ws () in
      if matches "val"        then (consume "val"; mk_val (valbind())) else
      if matches "type"       then (consume "type"; mk_type(typebind())) else
      if matches "datatype"   then (consume "datatype"; 
                                             mk_datatype (databind())) else
      if matches "fun"        then (consume "fun"; mk_fun (funbind())) else
      if matches "open"       then (consume "open"; mk_open(longid())) else
      if matches "nonfix"     then (consume "nonfix"; mk_nonfix(id_list())) 
      else if matches "infixr"     then (consume "infixr"; ws();
                  if is_digit () then mk_infixr(SOME (digit()),id_list())
                                 else mk_infixr(NONE,id_list())
      if matches "infix"      then (wconsume "infix"; 
                  if is_digit () then mk_infix(SOME (digit()),id_list())
                                 else mk_infix(NONE,id_list())
      if matches "exception"  then (consume "exception"; mk_excep(exnbind())
      else if matches "local" then 
         let
            val _ = consume "local"
            val dec1 = decl_list ()
            val _ = wconsume "in"
            val dec2 = dec_list ()
            val _ = wconsume "end"
         in
            mk_local (dec1, dec2)
         end
      else error "Expected declaration"
   end

   fun program_el () = 
      (* FIXME: Add structure/sig/functor rules *)
      decl ()

   fun program () =
      let
         val _ = ws ()
         val pl = program_el ()
         val _ = ws ()
      in
         if eof () then return [pl]
         else if matches ";" then (consume ";"; pl :: program ())
         else pl :: program ()
      end
            
end
