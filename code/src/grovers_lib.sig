(** Helper functions for Grovers algorithm *)

signature GROVERS_LIB = sig
	type circuit

	val oracleNaive    : int -> t
    val diffusionNaive :
