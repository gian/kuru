open Compiler.Internal

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

structure CommandLine =
struct

end

val true = 1 = 1
val false = 1 = 0

datatype 'a option = SOME of 'a | NONE

fun valOf NONE = raise Match
  | valOf (SOME x) = x

type 'a ref = opaque

_import k_int_add : (int * int) -> int as +
_import k_int_sub  : (int * int) -> int as -
_import k_int_mul : (int * int) -> int as *
_import k_int_leq : (int * int) -> bool as <=

_import k_ref_newref : 'a -> 'a ref as ref
_import k_ref_assign : ('a ref * 'a) -> unit as :=
_import k_ref_deref : 'a ref -> 'a as !

_import k_list_cons : ('a * 'a list) -> 'a list as ::
_import k_string_strlen : string -> int as strlen
(*_import k_string_strcat : string * string -> string as ^ *)

