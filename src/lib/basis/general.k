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

structure CommandLine =
struct

end

datatype option = SOME of 'a | NONE

