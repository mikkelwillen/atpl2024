structure Grovers : GROVERS = struct

	open Circuit

    val oo = op Seq
	val ** = op Tensor

	infix oo
	infix **

	(* helper function for creating `n` hadamard gates *)
	fun hadamard 1 = H
	  | hadamard n = H ** hadamard (n - 1)

	(* helper function for creating `n - 1` NOTs on a Z gate *)
	fun cxZ 1 = Z
	  | cxZ n = C (cxZ (n - 1))

    (* helper function for flipping the sign so that the target qubit is |1..1> *)
	fun flipSign n numQubits =
		if n = 0 then
			if numQubits > 1 then
			   (flipSign 0 (numQubits - 1)) ** X
			else if numQubits = 1 then
				X
		else if n = 1 then
			if numQubits > 1 then
				(flipSign 0 (numQubits - 1)) ** I
			else if numQubits = 1 then
				I
		else
			if n % 2 = 0 then
				(flipSign (n / 2) (numQubits - 1)) ** X
			else
				(flipSign (n / 2) (numQubits - 1)) ** I

	(* oracleNaive function
		- `n` is the target value
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the oracle function to the input state *)
	fun oracleNaive n numQubits : t =
		let val flip = flipSign n numQubits
		in
			flip oo cxZ numQubits oo flip
		end

	(* diffusionNaive function
		- `n` is the target value
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the diffusion operator to the input state *)
	fun diffusionNaive n numQubits : t =
		cxZ numQubits

	(* groversNaive function
		- `n` is the target value
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the grovers algorithm to the input state *)
	fun groversNaive n numQubits : t =
		let val hadamardGates = hadamard numQubits
		in
			hadamardGates oo oracleNaive n numQubits oo hadamardGates
		end

end
