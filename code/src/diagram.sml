
signature DIAGRAM =
sig
  type t
  val box      : string -> t        (* dim 1 *)
  val box2     : string -> t        (* dim 1 *)
  val line     : t                  (* dim 1 *)
  val cntrl    : string -> t        (* dim 2 *)
  val cntrln   : string -> int -> t (* dim n + 1 *)
  val swap     : t                  (* dim 2 *)
  val par      : t * t -> t         (* dim(seq(a,b)) = max(dim a, dim b) *)
  val seq      : t * t -> t         (* dim(par(a,b)) = dim a + dim b *)
  val toString : t -> string
end

structure Diagram : DIAGRAM =
struct
  type t = string list  (* lines; invariant: lines have equal size *)

  fun spaces i = CharVector.tabulate(i, fn _ => #" ")

  fun mapi2 f p =
      #2(ListPair.foldr (fn (a,b,(i,r)) => (i+1,f(i,a,b)::r)) (0,nil) p)

  fun mapi f x =
      #2(foldr (fn (a,(i,r)) => (i+1,f(i,a)::r)) (0,nil) x)

  fun width nil = 0
    | width (x::_) = size x

  (* control-circuit with n controls
     calls itself recursively *)
  fun cntrlnCompact (s:string) (n:int) : t =
      if n = 1
        then ["*", "|", s]
        else ["*", "|"] @ cntrlnCompact s (n-1)

 fun cntrlnHelper (s:string) (n:int) : t =
      if n = 1
        then ["--*--", "  |  ",
                        "  |  ", ".-+-.","| " ^ s ^ " |", "'---'"]
        else  ["--*--", "  | ", "  | ", "  | "] @ cntrlnHelper s (n-1)

  fun cntrln (s:string) (n:int) : t =
      ["     "] @ cntrlnHelper s n

  val compact_p = truSteanee

  val {box   : string -> t,      (* gate *)
       box2  : string -> t,      (* gate *)
       line  : t,                (* line *)
       cntrl : string -> t,      (* control-circuit *)
       cntrln: string -> int -> t, (* control-circuit with n controls *)
       swap  : t,                (* swap two qubits *)
       sep   : int -> string     (* index-variant separator *)
      } =
      if compact_p then
        {box=fn s => [s],
         box2=fn s => [s],
         line=["-"],
         cntrl=fn s => ["*", "|", s],
         cntrln=cntrlnCompact,
         swap=[". .", " X ", "' '"],
         sep=fn i => if i mod 2 = 0 then "-" else " "}
      else
        {box=fn s => [".---.","| " ^ s ^ " |", "'---'"],
         box2=fn s => [".---.","|" ^ s ^ " |","'---'"],
         line=["     ","-----", "     "],
         cntrl=fn s => ["     ", "--*--", "  |  ",
                        "  |  ", ".-+-.","| " ^ s ^ " |", "'---'"],
         cntrln=cntrln,
         swap=["     ", ".   .",
               " \\ / ", "  /  ", " / \\ ",
               "'   '", "     "],
         sep=fn i => if (i-1) mod 4 = 0 then "-" else " "}

  (* index-variant padding *)
  fun padi w (i,s) =
      if size s >= w then s
      else if size s = w-1 then s ^ sep i
      else padi w (i,sep i ^ s ^ sep i)

  fun par (a:t,b:t) : t =
      let val i = width a
          val j = width b
          val w = Int.max(i,j)
      in mapi (padi w) a @ [spaces w] @
         mapi (padi w) b
      end
  fun seq (a:t,b:t) : t = mapi2 (fn (i,a,b) => a ^ sep i ^ b) (a,b)
  fun toString (a:t) : string =
      let val a = mapi (padi (width a + 4)) a
      in String.concatWith "\n" a
      end
end
