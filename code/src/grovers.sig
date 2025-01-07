(** Grovers algorithm *)

signature GROVERS = sig

	val groversNaive  : int -> int -> Circuit.t
    val oracleNaive   : int -> int -> Circuit.t
	(* val diffuserNaive : int -> t *)

end
