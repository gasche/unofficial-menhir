(* These are the operations required of lookahead sets during
   a closure computation. This signature is exploited by the
   functor [Item.Closure]. *)

module type S = sig

  (* The type of lookahead sets. *)
  type t

  (* The empty lookahead set. Redundant with the following, but
     convenient. *)
  val empty: t

  (* A concrete, constant set of terminal symbols. *)
  val constant: Grammar.TerminalSet.t -> t

  (* [union s1 s2] returns the union of [s1] and [s2].  *)
  val union: t -> t -> t

end

