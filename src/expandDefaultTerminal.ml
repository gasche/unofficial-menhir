open Syntax
open UnparameterizedSyntax

(*let prepend producer branches =
  List.map
    (fun (producers,action) -> (producer :: producers, action))
    branches

let compose producer ident expr branches =
  List.map
    (fun (producers,action) ->
       (producer :: producers,
        Action.compose ident expr action))
    branches

let rec expand_producers position token_properties action = function
  | [] -> [[], action]
  | producer :: producers ->
    let branches = expand_producers (position + 1) token_properties action producers in
    prepend producer branches @
    begin match
        (try Some (StringMap.find (fst producer) token_properties)
         with Not_found -> None)
      with
      | None | Some { Syntax. tk_default_expr = None } -> []
      | Some { Syntax. tk_default_expr = Some expr } ->
        let ident = match producer with
          | _, Some ident -> ident
          | _, None -> "_" ^ (string_of_int position)
        in
        compose ("DEFAULT", snd producer) ident expr branches
    end*)

let skip_branch _grammar branch =
  Action.use_dollar branch.action || List.mem_assoc "DEFAULT" branch.producers

let fresh_precedence_level branch =
  match branch.branch_reduce_precedence with
  | PrecedenceLevel (m,_,p1,p2) ->
    PrecedenceLevel (m,ParserAux.current_reduce_level (),p1,p2)
  | _ -> assert false

let default_branch grammar nonterminal rule =
  try
    let action, position = StringMap.find nonterminal grammar.default_exprs in
    let precedence =
      match rule.branches with
      | [] -> ParserAux.current_reduce_precedence ()
      | branch :: _ -> fresh_precedence_level branch
    in
    let branch =
      {
        action; producers = [("DEFAULT", None)];
        branch_position = position;
        branch_shift_precedence = None;
        branch_reduce_precedence = precedence;
      }
    in
    { rule with branches = branch :: rule.branches }
  with Not_found ->
    rule

let split_producers grammar producers =
  let rec take_suffix suffix = function
    | (sym, _) :: _ as prefix
      when not (StringMap.mem sym grammar.default_exprs) ->
      prefix, suffix
    | [] -> [], suffix
    | producer :: producers ->
      take_suffix (producer :: suffix)  producers
  in
  let prefix, suffix = take_suffix [] (List.rev producers) in
  List.rev prefix, List.rev suffix

let kw_start = Keyword.KeywordSet.singleton
    (Keyword.Position (Keyword.RightNamed "_default_",
                       Keyword.WhereStart,
                       Keyword.FlavorPosition))

let kw_end = Keyword.KeywordSet.singleton
    (Keyword.Position (Keyword.RightNamed "_default_",
                       Keyword.WhereEnd,
                       Keyword.FlavorPosition))

let subst_by_default grammar symbol action =
  match symbol with
  | (_, None) -> action
  | (symbol, Some id) ->
    let keywords = Action.keywords action in
    let has where =
      Keyword.KeywordSet.mem
        Keyword.(Position (RightNamed id, where, FlavorOffset))
        keywords ||
      Keyword.KeywordSet.mem
        Keyword.(Position (RightNamed id, where, FlavorPosition))
        keywords
    in
    let action =
      Action.compose id
        (fst (StringMap.find symbol grammar.default_exprs))
        action
    in
    let action =
      if has Keyword.WhereEnd then
        Action.compose
          ("_endpos_" ^ id ^ "_")
          (Action.from_il_expr
             ~keywords:kw_start
             (IL.EVar "_endpos__default__"))
          action
      else action
    in
    let action =
      if has Keyword.WhereStart then
        Action.compose
          ("_startpos_" ^ id ^ "_")
          (Action.from_il_expr
             ~keywords:kw_start
             (IL.EVar "_startpos__default__"))
          action
      else action
    in
    action

let rec expand_producers grammar action producers =
  match producers with
  | [] -> []
  | p :: tail ->
    let action' =
      List.fold_right (subst_by_default grammar) producers action in
    (["DEFAULT", Some "_default_"], action') ::
    List.map
      (fun (ps,action) -> p :: ps, action)
      (expand_producers grammar action tail)

let expand_branch grammar branch =
  if Action.use_dollar branch.action then
    Error.warning [branch.branch_position]
      "Cannot mix $i syntax and default expressions declaration.";
  if skip_branch grammar branch then
    [branch]
  else
    match split_producers grammar branch.producers with
    | _, [] ->
      Error.warning [branch.branch_position]
        (Printf.sprintf "No default applicable to branch.");
      [branch]
    | prefix, suffix ->
      let binding = function
        | name, None -> name
        | name, Some id -> id ^ " = " ^ name
      in
      if List.length prefix > 0 then
        Error.warning [branch.branch_position]
          (Printf.sprintf "No default in the following prefix: \"%s\""
             (String.concat " " (List.map binding prefix)));
      let expansions = expand_producers grammar branch.action suffix in
      branch ::
      List.map (fun (producers,action) ->
          {
            branch with
            action;
            producers = prefix @ producers;
            branch_reduce_precedence = fresh_precedence_level branch;
          }) expansions

let expand_rule grammar nonterminal rule =
  if StringMap.mem nonterminal grammar.default_exprs then
    { rule with
      branches = List.flatten (List.map (expand_branch grammar) rule.branches) }
  else
    rule

let check grammar =
  (* Check that all default expressions apply to valid symbols *)
  StringMap.iter (fun symbol (_,position) ->
      if not (StringMap.mem symbol grammar.tokens) &&
         not (StringMap.mem symbol grammar.rules) then
        Error.warning [position]
          (Printf.sprintf "unused symbol %s in default expression declaration." symbol);
      if try (StringMap.find symbol grammar.rules).inline_flag
        with Not_found -> false
      then
        Error.warning [position]
          (Printf.sprintf "inlined rule %s can't have default expression." symbol);
      if symbol = "DEFAULT" then
        Error.error [position] "DEFAULT symbol can't default expression."
    ) grammar.default_exprs

let expand grammar =
  let rules = grammar.rules in

  (* 1. Generate default branch for non-terminals *)
  let rules = StringMap.mapi (default_branch grammar) rules in
  (* 2. Generate new branches when possible *)
  let rules = StringMap.mapi (expand_rule grammar) rules in

  { grammar with rules }
