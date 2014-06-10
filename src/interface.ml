open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

include PreInterface

(* ------------------------------------------------------------------------ *)

(* Definitions to be exported for step-by-step engine interface. *)

let ty ?(p=[]) n = TypApp (n,p)

let tytokentuple =
  TypTuple [
    ty "Lexing.position";
    TokenType.ttoken;
    ty "Lexing.position";
  ]

let steptypdefs =
  let tyenv = ty ~p:[ty "state"; ty "symbol"; TokenType.ttoken]
      "MenhirLib.EngineTypes.env"
  in
  [
    { typename = "state"; typeprivate = false;
      typerhs = TDefSum [];
      typeparams = []; typeconstraint = None };
    { typename = "feed"; typeprivate = false;
      typerhs = TAbbrev (ty "[ `Feed | `Feed_error ]");
      typeparams = []; typeconstraint = None };
    { typename = "step"; typeprivate = false;
      typerhs = TAbbrev (ty "[ `Step_run | `Step_error | `Step_action ]");
      typeparams = []; typeconstraint = None };
    { typename = "parser"; typeprivate = false;
      typerhs = TDefRecord [
          { modifiable = false;
            fieldname = "env";
            fieldtype = type2scheme tyenv;
          };
          { modifiable = false;
            fieldname = "tag";
            fieldtype = type2scheme (TypVar "a");
          };
        ];
      typeparams = ["a"]; typeconstraint = None };
  ]

let stepvaldecl =
  let result =
    "  [ `Step of step parser\n\
    \  | `Feed of feed parser\n\
    \  | `Accept of symbol\n\
    \  | `Reject ]"
  in
  [
    "initial", { quantifiers = [];
                 body = arrow (ty "state")
                          (arrow tytokentuple (ty ~p:[ty "step"] "parser")) };
    "step",    { quantifiers = [];
                 body = arrow (ty ~p:[ty "step"] "parser") (ty result) };
    "feed",    { quantifiers = [];
                 body = arrow (ty ~p:[ty "feed"] "parser")
                          (arrow tytokentuple (ty ~p:[ty "step"] "parser")) };
  ]

let typedefs =
  PreInterface.interface.typedecls @
  if Settings.typed_values then
    let symboldefs =
      let add_n sym ocamltype datadefs =
        {
          dataname = "N_" ^ Misc.normalize sym;
          datavalparams = [];
          datatypeparams = Some [ TypTextual ocamltype ];
        } :: datadefs
      in
      let add_t sym properties datadefs =
        if properties.Syntax.tk_is_declared then
          let params = match properties.Syntax.tk_ocamltype with
            | None   -> [ TypApp ("unit",[]) ]
            | Some t -> [ TypTextual t ]
          in
          {
            dataname = "T_" ^ sym;
            datavalparams = [];
            datatypeparams = Some params;
          } :: datadefs
        else
          datadefs
      in
      let n_defs = StringMap.fold add_n Front.grammar.types [] in
      let t_defs = StringMap.fold add_t Front.grammar.tokens [] in
      [
        {
          typename = "token_class";
          typeparams = ["_"];
          typerhs = TDefSum t_defs;
          typeconstraint = None;
          typeprivate = false;
        };
        {
          typename = "nonterminal_class";
          typeparams = ["_"];
          typerhs = TDefSum n_defs;
          typeconstraint = None;
          typeprivate = false;
        }
      ]
    in
    let symbolclassdef =
      {
        typename = "symbol_class";
        typeparams = [];
        typerhs = TDefSum [
            {
              dataname = "CT_";
              datavalparams = [TypApp ("token_class",[TypVar "a"])];
              datatypeparams = Some [];
            };
            {
              dataname = "CN_";
              datavalparams = [TypApp ("nonterminal_class",[TypVar "a"])];
              datatypeparams = Some [];
            };
          ];
        typeconstraint = None;
        typeprivate = false;
      }
    in
    let symboltypedef =
      {
        typename = "symbol";
        typeparams = [];
        typerhs = TDefSum [
            {
              dataname = "T_";
              datavalparams = [TypApp ("token_class",[TypVar "a"]); TypVar "a"];
              datatypeparams = Some [];
            };
            {
              dataname = "N_";
              datavalparams = [TypApp ("nonterminal_class",[TypVar "a"]); TypVar "a"];
              datatypeparams = Some [];
            };
            {
              dataname = "Bottom";
              datavalparams = [];
              datatypeparams = None;
            };
          ];
        typeconstraint = None;
        typeprivate = false;
      }
    in
    symboldefs @ [symbolclassdef; symboltypedef]
  else if Settings.stepwise then
    [ { typename = "symbol";
        typerhs = TAbbrev (TypApp ("Obj.t", []));
        typeparams = []; typeconstraint = None; typeprivate = false } ]
  else
    []

let typedecls =
  typedefs @
  if Settings.stepwise
  then steptypdefs
  else []

let valdecls =
  PreInterface.interface.valdecls @
    if Settings.stepwise then
      let stepentryvaldecls =
        StringSet.fold (fun symbol decls ->
            (Misc.normalize symbol ^ "_state",
             {quantifiers = []; body = TypApp ("state",[])}) :: decls
          ) PreFront.grammar.start_symbols []
      in
      stepvaldecl @ stepentryvaldecls
    else []

let querydef = [
  "";
  "module Query : MenhirLib.EngineTypes.QUERY_ENGINE";
  "   with type state := state";
  "    and type production := int";
  "    and type producer := symbol_class";
  "    and type semantic_action =";
  "               (state, symbol, token) MenhirLib.EngineTypes.env ->";
  "               (state, symbol) MenhirLib.EngineTypes.stack";
]

let interface =
  { PreInterface.interface with
    typedecls = typedecls;
    valdecls = valdecls;
    intf_footer =
      if Settings.typed_values then
        querydef
      else
        []
    ;
  }

(* Writing the interface to a file. *)

let write () =
  let mli = open_out (Settings.base ^ ".mli") in
  let module P = Printer.Make (struct
    let f = mli
    let locate_stretches = None
    let raw_stretch_action = false
  end) in
  P.interface interface;
  close_out mli

