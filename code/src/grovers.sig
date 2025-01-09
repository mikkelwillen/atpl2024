(** Grovers algorithm *)

signature GROVERS = sig

	val groversNaive  : int -> int -> Circuit.t
	val initKets      : int -> int -> Semantics.ket

end
