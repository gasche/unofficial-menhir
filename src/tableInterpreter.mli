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

module MakeQuery (T : TableFormat.TABLES) (Q : TableFormat.QUERY_TABLE)
  : EngineTypes.QUERY_ENGINE with type producer = Q.producer_definition
                              and type production = int
                              and type annotation = Q.annotation_definition
                              and type semantic_action =
                                (int, T.semantic_value, T.token) EngineTypes.env ->
                                (int, T.semantic_value) EngineTypes.stack
