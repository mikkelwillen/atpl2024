structure Circuit : CIRCUIT = struct

  infix |> fun a |> f = f a

  datatype t = I | X | Y | Z | H | SW | T | TI
             | Tensor of t * t
             | Seq of t * t
             | C of t

  val oo = op Seq
  val ** = op Tensor

  infix oo
  infix **

  fun pp t =
      let fun maybePar P s = if P then "(" ^ s ^ ")" else s
          fun pp p t =
              case t of
                  I => "I"
                | X => "X"
                | Y => "Y"
                | Z => "Z"
                | H => "H"
                | SW => "SW"
                | T => "T"
                | TI => "TI"
                | Tensor(t1,t2) => maybePar (p > 4) (pp 4 t1 ^ " ** " ^ pp 4 t2)
                | Seq(t1,t2) => maybePar (p > 3) (pp 3 t1 ^ " oo " ^ pp 3 t2)
                | C t => "C" ^ pp 8 t
    in pp 0 t
    end

  fun lookC t : (string * int) option =
      case t of
          I => NONE
        | C X => SOME ("X", 1)
        | C Y => SOME ("Y", 1)
        | C Z => SOME ("Z", 1)
        | C H => SOME ("H", 1)
        | C T => SOME ("T", 1)
        | C TI => SOME ("TI", 1)
        | C t => (case lookC t of
                      NONE => NONE
                    | SOME (s, n) => SOME (s, n + 1)
                  )
        | Tensor(a,b) => NONE
        | Seq(a,b) => NONE
        | SW => NONE

  fun draw t =
      let fun dr t =
              case t of
                  SW => Diagram.swap
                | Tensor(a,b) => Diagram.par(dr a, dr b)
                | Seq(a,b) => Diagram.seq(dr a, dr b)
                | I => Diagram.line
                | X => Diagram.box "X"
                | Y => Diagram.box "Y"
                | Z => Diagram.box "Z"
                | H => Diagram.box "H"
                | T => Diagram.box "T"
                | TI => Diagram.box2 "TI"
                | _ => case lookC t of
                           NONE => raise Fail ("Circuit.draw: Unknown gate " ^ pp t)
                         | SOME (s, n) => Diagram.cntrln s n
      in dr t |> Diagram.toString
      end

  fun dim t =
      case t of
          Tensor(a,b) => dim a + dim b
        | Seq(a,b) =>
          let val d = dim a
          in if d <> dim b
             then raise Fail "Sequence error: mismatching dimensions"
             else d
          end
        | SW =>  2
        | C t => 1 + dim t
        | _ => 1

  (* auxiliary function for creating a circuit with n parallel
     `I` gates *)
  fun id 1 = I
    | id n = I ** id (n - 1)


  (* helper function for creating a one-layer circuit
     of height k and swaps qubit n and n-1 *)
  fun swap1 2 1 = SW
    | swap1 k 1 = SW ** id (k - 2)
    | swap1 k n = if n = k + 1
                    then id (k - 1) ** SW
                    else id (n - 1) ** SW ** id (k - n - 1)

  (* recursive function for creating a circuit with n qubits *)
  (* recursive function for swapping qubit 0 with qubit n in a circuit *)
  (* h is the height of the circuit, n is the qubit to swap with 0 *)
  fun swap h 1 = swap1 h 1
    | swap h n = swap h (n - 1) oo swap1 h n

  (* Optimise
     Optimises a circuit by removing redundant gates such as
     `I oo X` or `X oo I` or `H oo H` etc. *)
  fun optimise t =
      case t of
          Seq(I, t) => optimise t
        | Seq(t, I) => optimise t
        | Seq(X, X) => optimise I
        | Seq(Y, Y) => optimise I
        | Seq(Z, Z) => optimise I
        | Seq(H, H) => optimise I
        | Seq(T, T) => optimise I
        | Seq(TI, TI) => optimise I
        | Seq(a, b) => Seq(optimise a, optimise b)
        | Tensor(a, b) => Tensor(optimise a, optimise b)
        | C t => C(optimise t)
        | _ => t

  (* Inverse
     Returns the inverse of a circuit *)
  fun inverse t =
      case t of
          T => TI
        | TI => T
        | Seq(a, b) => Seq(inverse b, inverse a)
        | Tensor(a, b) => Tensor(inverse a, inverse b)
        | C t => C (inverse t)
        | _ => t
end
