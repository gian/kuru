type foo = string list

type bar = (string * int) list list list list list list ref

type res_program = int list * int * foo list

type t2 = (int * (int * res_program)) list ref
