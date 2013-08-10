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

fun a + b = 0
(* fun {A : ADDABLE 'a} + (= add (a, b) *)

_import intAdd : (int * int) -> int as +
_import intSub  : (int * int) -> int as -
_import intMul : (int * int) -> int as *
_import intLeq : (int * int) -> bool as <=

_import refNewref : 'a -> 'a ref as ref
_import refAssign : ('a ref * 'a) -> unit as :=
(* _import k_ref_deref : 'a ref -> 'a as ! *)

_import listCons : ('a * 'a list) -> 'a list as ::
_import stringStrlen : string -> int as strlen
(*_import k_string_strcat : string * string -> string as ^ *)

