(** This is a test.
	@author Gian Perrone *)
structure Foo =
struct
	(** The initial value *)
	val initial = 234

	(** A function *)
	fun f x = x

	(* A function without a doc string *)
	fun g x = x

	(** Another function with a doc string *)
	fun h 0 = 0
	  | h 1 = 1
	  | h x = x

	and (** And yet another one *)
	    i 0 = 0
	  | i x = x

end