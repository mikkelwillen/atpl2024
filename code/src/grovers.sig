(** Grovers algorithm *)

signature GROVERS = sig

	val groversNaive      	 : int -> int -> Circuit.t * Semantics.ket
	val groversOracleFun 	 : (int -> Circuit.t) -> int -> Circuit.t * Semantics.ket
    val groversOracleCircuit : Circuit.t -> int -> Circuit.t * Semantics.ket

    val oracleNaive          : int -> int -> Circuit.t
	val initKets      		 : int -> int -> Semantics.ket

end
