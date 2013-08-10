
(** Generated from string.idl *)
signature STRING =
sig

	(** concat : lhs:string -> rhs:string -> string *)
	val concat : string -> string -> string

	(** strlen : str:string -> int *)
	val strlen : string -> int

	(** strcmp : lhs:string -> rhs:string -> int *)
	val strcmp : string -> string -> int

end

structure string : STRING =
struct

	(** concat : lhs:string -> rhs:string -> string *)
	_import kconcat : string -> string -> string as concat

	(** strlen : str:string -> int *)
	_import kstrlen : string -> int as strlen

	(** strcmp : lhs:string -> rhs:string -> int *)
	_import kstrcmp : string -> string -> int as strcmp
end
