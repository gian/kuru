
(** Generated from io.idl *)
signature IO =
sig
 
	(** print : message:string -> cvoid *)
	val print : string -> cvoid
 
	(** getch : unit -> int *)
	val getch : unit -> int
 
	(** cputs : message:string -> cvoid *)
	val cputs : string -> cvoid

end

structure io : IO =
struct
 
	(** print : message:string -> cvoid *)
	_import kuru_print : string -> cvoid as print
 
	(** getch : unit -> int *)
	_import getchar : unit -> int as getch
 
	(** cputs : message:string -> cvoid *)
	_import puts : string -> cvoid as cputs
end
