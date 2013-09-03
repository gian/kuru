
infix 7 *
infix 7 /
infix 7 mod
infix 7 div
infix 6 +
infix 6 -
infix 6 ^
infixr 5 ::
infixr 5 @
infix 4 <>
infix 4 >
infix 4 >=
infix 4 <
infix 4 <=
infix 3 :=
infix 3 o
infix 2 orelse
infix 2 andalso
infix 0 before

exception Match
exception Fail of string

datatype bool = true | false

datatype 'a option = SOME of 'a | NONE

fun valOf NONE = raise Match
  | valOf (SOME x) = x

datatype 'a ref = ref of 'a

fun ! (ref x) = x
fun ^ (a : string, b : string) : string = ""
fun - (a, b) = 0
fun + (a, b) = 0
fun * (a, b) = 0
fun / (a, b) = 0.0
fun @ (a,b) = b
fun div (a, b) = 0
fun <= (a, b) = false
fun >= (a, b) = false
fun < (a, b) = false
fun > (a, b) = false
fun <> (a, b) = false

fun andalso (true, true) = true
  | andalso _ = false

fun orelse (false, false) = false
  | orelse _ = true

fun not true = false
  | not false = true

fun mod (0,0) = 0

fun o (g, f) = fn x => g (f x)

datatype 'a list = nil | :: of 'a * 'a list

fun internal_print (s : string) = ()

(* fun builtin_add {A : 'a Addable} (x : 'a, y : 'a) = A.add (a, b) *)

fun a before b = a

fun := ((ref a) : 'a ref, b : 'a) = () 

_import intSub  : (int * int) -> int as -
_import intMul : (int * int) -> int as *
_import intLeq : (int * int) -> bool as <=

_import refNewref : 'a -> 'a ref as ref
_import refAssign : ('a ref * 'a) -> unit as :=
(* _import k_ref_deref : 'a ref -> 'a as ! *)

_import listCons : ('a * 'a list) -> 'a list as ::
_import stringStrlen : string -> int as strlen
(*_import k_string_strcat : string * string -> string as ^ *)

structure CommandLine =
struct
   fun arguments () = "foo" :: nil
end


