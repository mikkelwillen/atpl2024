# Implementing Quantum Algorithms
Mini-project for ATPL course 2024 by Caroline Kierkegaard and Mikkel Willén

## Abstract
Quantum computing promises significant advancements in computational power, enabling solutions to problems that are intractable for classical systems. Among the foundational quantum algorithms, Grover's search algorithm stands out for its ability to provide a quadratic speedup in searching unstructured databases. In this work, we implemented and simulated Grover's algorithm within the Standard ML (SML) framework, extending its capabilities to evaluate quantum algorithms on classical systems. Our implementation included the design and integration of key components such as the oracle and diffusion operators. The framework's accuracy was validated against the Quirk quantum simulator, showing consistent results. Experimental evaluation demonstrated the scalability of the implementation up to 16 qubits, highlighting both the promise and the limitations of classical simulation approaches. This project establishes a foundation for future extensions of the SML framework to support a broader range of quantum algorithms and provides insights into the challenges of hybrid quantum-classical computing.

<<<<<<< HEAD
## Code structure
In the `code` directory, find the `src` directory
``` sh
src
├── circuit.sig
├── circuit.sml
├── comp_ex1.mlb
├── comp_ex1.sml
├── complex.sig
├── complex.sml
├── comp.sml
├── diagram.sml
├── fut/
├── futhark.sml
├── futlib.fut
├── grovers_circuit_test.mlb
├── grovers_circuit_test.sml
├── grovers_fun_test.mlb
├── grovers_fun_test.sml
├── grovers_interp.mlb
├── grovers_interp.sml
├── grovers.mlb
├── grovers.sig
├── grovers.sml
├── grovers_test.mlb
├── grovers_test.sml
├── Makefile
├── MLB/
├── quantum_ex1.mlb
├── quantum_ex1.sml
├── quantum_ex2.mlb
├── quantum_ex2.sml
├── quantum_ex4.mlb
├── quantum_ex4.sml
├── quantum_ex6.mlb
├── quantum_ex6.sml
├── quantum_ex7.mlb
├── quantum_ex7.sml
├── quantum_ex8.mlb
├── quantum_ex8.sml
├── quantum.mlb
├── run
├── semantics.sig
├── semantics.sml
└── test/
```

## How to run the code
In the `src` directory, run the following commands:
``` sh
# Run the standard test
$ mlkit grovers_test.mlb
$ ./run

# Run the test where the oracle is given as a function
$ mlkit grovers_fun_test.mlb
$ ./run

# Run the test where the oracle is given as a circuit
$ mlkit grovers_circuit_test.mlb
$ ./run
```
=======
>>>>>>> 004b7212350f18a7f0b8a5900e80596e3113ea51
