(** Grovers algorithm *)

signature GROVERS = sig
	type complex
    type circuit

	val groversNaive : int -> t

end
