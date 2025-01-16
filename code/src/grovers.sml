structure Grovers : GROVERS = struct

	open Circuit
	open Semantics

	val oo = op Seq
	val ** = op Tensor

	infix 3 oo
	infix 4 **
	infix div
	infix mod

	(* integer power *)
	fun powInt 0 y = 0
	  | powInt x 0 = 1
	  | powInt x y = x * powInt x (y - 1)

	(* helper function for creating `n` hadamard gates *)
	fun hadamard 1 = H
	  | hadamard n = H ** hadamard (n - 1)

   	(* helper function for creating `n - 1` hadamard gates and I on ancilla *)
	fun hadamardI 1 = I
	  | hadamardI n = H ** hadamardI (n - 1)

	(* helper function for creating `n - 1` X gates and I on ancilla *)
	fun notI 1 = I
	  | notI n = X ** notI (n - 1)

	(* helper function for creating a Z gate on the ancilla bit *)
	fun zAncilla 1 = Z
	| zAncilla n = I ** zAncilla (n - 1)

	(* helper function for creating `n - 1` NOTs on a Z gate *)
	fun cxNot 1 = X
	  | cxNot n = C (cxNot (n - 1))

	(* helper function for flipping the bits so that the target qubit is |1..1> *)
	fun flipBits n numQubits : t =
		if (n > (powInt 2 numQubits) - 1) then
			raise Fail ("does not work" ^ (Int.toString n))
		else if n = 0 then
			if numQubits > 1 then
				(flipBits 0 (numQubits - 1)) ** X
			else if numQubits = 1 then
				X
			else
				raise Fail "Invalid number of qubits, n = 0"
		else if n = 1 then
			if numQubits > 1 then
				(flipBits 0 (numQubits - 1)) ** I
			else if numQubits = 1 then
				I
			else
				raise Fail "Invalid number of qubits, n = 1"
		else
			if (n mod 2) = 0 then
				(flipBits (n div 2) (numQubits - 1)) ** X
			else
				(flipBits (n div 2) (numQubits - 1)) ** I

    (* from: https://gist.github.com/edalorzo/4670775 *)
	(* Replicates an element x a number of n times and returns a list. *)
	fun replicate(x, n) =
		if n < 1 then
			[]
		else
			x::replicate(x, n-1)

	(* function for initializing the state to |0..0> with `n` bits and `m`ancilla bits *)
    fun initKets n m : ket =
		let val numQubits = n + m
		in
			ket (replicate(0, numQubits))
		end

	(* oracleNaive function
		- `n` is the target value
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the oracle function to the input state *)
	fun oracleNaive n numQubits : t =
		let val flip = (flipBits n (numQubits - 1)) ** I
		in
			flip oo
			cxNot numQubits oo
			flip
		end

	(* diffusionNaive function
		- `n` is the target value
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the diffusion operator to the input state *)
	fun diffusionNaive n numQubits : t =
		hadamardI numQubits oo
		notI numQubits oo
		cxNot numQubits oo
		notI numQubits oo
		hadamardI numQubits

	(* repeatCircuit function that repeats the circuit `t` `n` times *)
	fun repeatCircuit t n =
		if n < 1 then
			raise Fail "Number of iterations must be greater than 0"
		else if n = 1 then
			t
		else
			t oo repeatCircuit t (n - 1)

	(* groversNaive function
		- `n` is the target value
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the grovers algorithm to the input state *)
	fun groversNaive n inputNumQubits : t * ket =
		let val numQubits = inputNumQubits + 1
			val iterations = Real.floor (Math.pi / 4.0 * Math.sqrt (Real.fromInt (powInt 2 inputNumQubits)))
			val hadamardGates = hadamard numQubits
			val initAncilla = zAncilla numQubits
			val oracle = oracleNaive n numQubits
			val diffusion = diffusionNaive n numQubits
			val repetition = repeatCircuit (oracle oo diffusion) iterations
		in
			(hadamardGates oo initAncilla oo repetition, initKets inputNumQubits 1)
		end


	(* grover function, where the oracle is given as a function *)
	(* groversOracleFun function
		- `oracle` is the oracle function
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the grovers algorithm to the input state *)
	fun groversOracleFun oracle inputNumQubits : t * ket =
		let val numQubits = inputNumQubits + 1
			val iterations = Real.floor (Math.pi / 4.0 * Math.sqrt (Real.fromInt (powInt 2 inputNumQubits)))
			val hadamardGates = hadamard numQubits
			val initAncilla = zAncilla numQubits
			val oracleCircuit = oracle numQubits
			val diffusion = diffusionNaive 0 numQubits
			val repetition = repeatCircuit (oracleCircuit oo diffusion) iterations
		in
			(hadamardGates oo initAncilla oo repetition, initKets inputNumQubits 1)
		end

	(* grover function, where the oracle is given as a circuit *)
	(* groversOracleCircuit function
		- `oracle` is the oracle circuit
		- `numQubits` is the number of qubits in the circuit

		- returns a circuit that applies the grovers algorithm to the input state *)
	fun groversOracleCircuit oracle inputNumQubits : t * ket =
		let val numQubits = inputNumQubits + 1
			val iterations = Real.floor (Math.pi / 4.0 * Math.sqrt (Real.fromInt (powInt 2 inputNumQubits)))
			val hadamardGates = hadamard numQubits
			val initAncilla = zAncilla numQubits
			val diffusion = diffusionNaive 0 numQubits
			val repetition = repeatCircuit (oracle oo diffusion) iterations
		in
			(hadamardGates oo initAncilla oo repetition, initKets inputNumQubits 1)
		end
end
