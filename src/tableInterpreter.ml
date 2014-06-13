(* This module instantiates the generic [Engine] with a thin decoding layer
   for the generated tables. Like [Engine], it is part of [MenhirLib]. *)

(* The exception [Accept] is no longer pre-declared here: with --typed-values,
   the Table backend now generates an exception parametrized over the type of
   semantic values. *)

(* This functor is invoked by the generated parser. *)

module Make (T : TableFormat.TABLES)

= Engine.Make (struct

  type state =
      int

  type token =
      T.token

  type terminal =
      int

  type semantic_value =
    T.semantic_value

  let token2terminal =
    T.token2terminal

  let token2value =
    T.token2value

  let error_terminal =
    T.error_terminal

  let error_value =
    T.error_value

  type production =
      int

  let default_reduction state defred nodefred env =
    let code = PackedIntArray.get T.default_reduction state in
    if code = 0 then
      nodefred env
    else
      defred env (code - 1)

  (* This auxiliary function helps access a compressed, two-dimensional
     matrix, like the action and goto tables. *)

  let unmarshal2 table i j =
    RowDisplacement.getget
      PackedIntArray.get
      PackedIntArray.get
      table
      i j

  (* This auxiliary function helps access a flattened, two-dimensional
     matrix, like the error bitmap. *)

  let unflatten (n, data) i j =
    PackedIntArray.get1 data (n * i + j)

  let action state terminal value shift reduce fail env =
    match unflatten T.error state terminal with
    | 1 ->
        let action = unmarshal2 T.action state terminal in
        let opcode = action land 0b11
        and param = action lsr 2 in
        if opcode >= 0b10 then
          (* 0b10 : shift/discard *)
          (* 0b11 : shift/nodiscard *)
          let please_discard = (opcode = 0b10) in
          shift env please_discard terminal value param
        else
          (* 0b01 : reduce *)
          (* 0b00 : cannot happen *)
          reduce env param
    | c ->
        assert (c = 0);
        fail env

  let goto state prod =
    let code = unmarshal2 T.goto state (PackedIntArray.get T.lhs prod) in
    (* code = 1 + state *)
    code - 1

  exception Accept = T.Accept

  exception Error = T.Error

  type semantic_action =
    (state, semantic_value, token) EngineTypes.env ->
     (state, semantic_value) EngineTypes.stack

  let semantic_action prod =
    T.semantic_action.(prod)

  let recovery =
    T.recovery

  module Log = struct

    open Printf

    let state state =
      match T.trace with
      | Some _ ->
          fprintf stderr "State %d:\n%!" state
      | None ->
          ()

    let shift terminal state =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Shifting (%s) to state %d\n%!" terminals.(terminal) state
      | None ->
          ()

    let reduce_or_accept prod =
      match T.trace with
      | Some (_, productions) ->
          fprintf stderr "%s\n%!" productions.(prod)
      | None ->
          ()

    let lookahead_token start_p token curr_p =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Lookahead token is now %s (%d-%d)\n%!"
            terminals.(token)
            start_p.Lexing.pos_cnum
            curr_p.Lexing.pos_cnum
      | None ->
          ()

    let initiating_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Initiating error handling\n%!"
      | None ->
          ()

    let resuming_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Resuming error handling\n%!"
      | None ->
          ()

    let handling_error state =
      match T.trace with
      | Some _ ->
          fprintf stderr "Handling error in state %d\n%!" state
      | None ->
          ()

    let discarding_last_token token =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Discarding last token read (%s)\n%!" terminals.(token)
      | None ->
          ()

  end

end)

module MakeQuery (T : TableFormat.TABLES) (Q : TableFormat.QUERY_TABLE) =
struct

  type lr1_state = int
  type lr0_state = int

  type production = int
  type producer = Q.producer_definition
  type semantic_action =
    (int, T.semantic_value, T.token) EngineTypes.env ->
    (int, T.semantic_value) EngineTypes.stack

  let lr0_states = Q.lr0_states
  let lr1_states = Q.lr1_states

  let productions = Array.length Q.productions_definition

  let lr0_state lr1 = PackedIntArray.get Q.lr0_mapping lr1
  let itemset lr0 = Q.lr0_itemset.(lr0)

  let production_definition prod =
    let lhs, rhs, _ = Q.productions_definition.(prod) in
    lhs, rhs

  let semantic_action prod =
    match Q.productions_definition.(prod) with
    | _, _, None -> None
    | _, _, Some action -> Some T.semantic_action.(action)

end
