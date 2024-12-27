signature CIRCUIT = sig

  datatype t = I | X | Y | Z | H | SW | T | TI
             | C of t
             | Tensor of t * t
             | Seq of t * t

  val oo    : t * t -> t
  val **    : t * t -> t

  val pp    : t -> string
  val draw  : t -> string
  val dim   : t -> int
  val id    : int -> t
  val swap1 : int -> int -> t
  val swap  : int -> int -> t
  val optimise : t -> t
  val inverse : t -> t

end
