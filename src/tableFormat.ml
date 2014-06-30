(* This signature defines the format of the parse tables. It is used as
   an argument to [TableInterpreter]. *)

module type TABLES = sig

  (* This is the parser's type of tokens. *)

  type token

  (* This is the type of parser's semantic values. *)

  type semantic_value

  (* This maps a token to its internal (generation-time) integer code. *)

  val token2terminal: token -> int

  (* This is the integer code for the error pseudo-token. *)

  val error_terminal: int

  (* Distinguished semantic_value tagging invalid stack frame *)

  val error_value: semantic_value

  (* This maps a token to its semantic value. *)

  val token2value: token -> semantic_value

  (* Traditionally, an LR automaton is described by two tables, namely, an
     action table and a goto table. See, for instance, the Dragon book.

     The action table is a two-dimensional matrix that maps a state and a
     lookahead token to an action. An action is one of: shift to a certain
     state, reduce a certain production, accept, or fail.

     The goto table is a two-dimensional matrix that maps a state and a
     non-terminal symbol to either a state or undefined. By construction, this
     table is sparse: its undefined entries are never looked up. A compression
     technique is free to overlap them with other entries.

     In Menhir, things are slightly different. If a state has a default
     reduction on token [#], then that reduction must be performed without
     consulting the lookahead token. As a result, we must first determine
     whether that is the case, before we can obtain a lookahead token and use it
     as an index in the action table.

     Thus, Menhir's tables are as follows.

     A one-dimensional default reduction table maps a state to either ``no
     default reduction'' (encoded as: 0) or ``by default, reduce prod''
     (encoded as: 1 + prod). The action table is looked up only when there
     is no default reduction. *)

  val default_reduction: PackedIntArray.t

  (* Menhir follows Dencker, D�rre and Heuft, who point out that, although the
     action table is not sparse by nature (i.e., the error entries are
     significant), it can be made sparse by first factoring out a binary error
     matrix, then replacing the error entries in the action table with undefined
     entries. Thus:

     A two-dimensional error bitmap maps a state and a terminal to either
     ``fail'' (encoded as: 0) or ``do not fail'' (encoded as: 1). The action
     table, which is now sparse, is looked up only in the latter case. *)

  (* The error bitmap is flattened into a one-dimensional table; its width is
     recorded so as to allow indexing. The table is then compressed via
     [PackedIntArray]. The bit width of the resulting packed array must be
     [1], so it is not explicitly recorded. *)

  (* The error bitmap does not contain a column for the [#] pseudo-terminal.
     Thus, its width is [Terminal.n - 1]. We exploit the fact that the integer
     code assigned to [#] is greatest: the fact that the right-most column
     in the bitmap is missing does not affect the code for accessing it. *)

  val error: int (* width of the bitmap *) * string (* second component of [PackedIntArray.t] *)

  (* A two-dimensional action table maps a state and a terminal to one of
     ``shift to state s and discard the current token'' (encoded as: s | 10),
     ``shift to state s without discarding the current token'' (encoded as: s |
     11), or ``reduce prod'' (encoded as: prod | 01). *)

  (* The action table is first compressed via [RowDisplacement], then packed
     via [PackedIntArray]. *)

  (* Like the error bitmap, the action table does not contain a column for the
     [#] pseudo-terminal. *)

  val action: PackedIntArray.t * PackedIntArray.t

  (* A one-dimensional lhs table maps a production to its left-hand side (a
     non-terminal symbol). *)

  val lhs: PackedIntArray.t

  (* A two-dimensional goto table maps a state and a non-terminal symbol to
     either undefined (encoded as: 0) or a new state s (encoded as: 1 + s). *)

  (* The goto table is first compressed via [RowDisplacement], then packed
     via [PackedIntArray]. *)

  val goto: PackedIntArray.t * PackedIntArray.t

  (* A one-dimensional semantic action table maps productions to semantic
     actions. The calling convention for semantic actions is described in
     [EngineTypes]. *)

  val semantic_action: ((int, semantic_value, token) EngineTypes.env ->
                        (int, semantic_value) EngineTypes.stack) array


  (* The parser defines its own [Accept] exception. This exception is raised
     and caught by the engine inside entry points to return a final result. *)

  exception Accept of semantic_value

  (* The parser defines its own [Error] exception. This exception can be
     raised by semantic actions and caught by the engine, and raised by the
     engine towards the final user. *)

  exception Error

  (* The parser indicates whether to perform error recovery. *)

  val recovery: bool

  (* The parser indicates whether to generate a trace. Generating a
     trace requires two extra tables, which respectively map a
     terminal symbol and a production to a string. *)

  val trace: (string array * string array) option

end

(* Low-level interface needed to query the engine *)

module type QUERY_TABLE = sig

  (* Number of states in lr0 and lr1 automata *)
  val lr0_states: int
  val lr1_states: int

  (* Mapping from lr1 state number to lr0 state number *)
  val lr0_mapping: PackedIntArray.t

  (* Mapping from lr0 state to the underlying item set.
     An item is a pair of a production and a position in this production. *)
  val lr0_itemset: (int * int) list array

  (* A production is a pair of a producer array and an optional reduction,
     represented by an index to an action in the [semantic_action] array.
     A reduction can be [None] if it had been removed dead code elimination.
  *)
  type producer_definition
  type annotation_definition

  val productions_definition:
    (producer_definition option *
     producer_definition list *
     (int option * annotation_definition list)) array

end
