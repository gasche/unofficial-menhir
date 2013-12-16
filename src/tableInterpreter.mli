(* This module instantiates the generic [Engine] with a thin decoding layer
   for the generated tables. Like [Engine], it is part of [MenhirLib]. *)

(* The exception [Accept] is no longer pre-declared here: with --typed-values,
   the Table backend now generates an exception parametrized over the type of
   semantic values. *)

(* This functor is invoked by the generated parser. *)

module Make (T : TableFormat.TABLES)

: EngineTypes.ENGINE with type state = int
                      and type token = T.token
                      and type semantic_value = T.semantic_value
                      and type terminal := int

