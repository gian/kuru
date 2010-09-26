(* Generated from kupeg.kpg *)

type st = { pos : int, va : string option }

fun push (stack, s : st option) = stack := s :: (!stack)

fun pop stack =
   let
      val s = !stack
      val _ = stack := tl s
   in
      hd s
   end


fun pos_ (s : st option ref) =
   let
      val s' = valOf (!s)
   in
      #pos s'
   end

fun va_ (s : st option ref) =
   let
      val s' = valOf (!s)
   in
      valOf (#va s')
   end

fun notnone s = case (!s) of NONE => false
                           | SOME x => true

fun kupeg_make_stack () = ref [] : st option list ref

fun kupeg_join l = String.concatWith "" l


