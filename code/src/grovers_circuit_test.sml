open Circuit Semantics Grovers
infix 3 oo
infix 4 **

fun run (c, k) =
    (print ("Circuit for c = " ^ pp c ^ ":\n");
     print (draw c ^ "\n");
     print ("Semantics of c:\n" ^ pp_mat(sem c) ^ "\n");
     print ("Result distribution when evaluating c on " ^ pp_ket k ^ " :\n");
     print (pp_dist(measure_dist_ancilla(eval c (init k)) 1) ^ "\n\n"))

val () = run (groversOracleCircuit (oracleNaive 3 4) 3)
