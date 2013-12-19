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
    let symbolclasstypedef =
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
      let datadefs =
        [ {
          dataname = "Bottom";
          datavalparams = [];
          datatypeparams = Some [ TypApp ("unit",[]) ];
        } ]
      in
      let datadefs = StringMap.fold add_n Front.grammar.types datadefs in
      let datadefs = StringMap.fold add_t Front.grammar.tokens datadefs in
      {
        typename = "symbol_class";
        typeparams = ["_"];
        typerhs = TDefSum datadefs;
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
              dataname = "Symbol";
              datavalparams = [TypApp ("symbol_class",[TypVar "a"]); TypVar "a"];
              datatypeparams = Some [];
            };
          ];
        typeconstraint = None;
        typeprivate = false;
      }
    in
    [symbolclasstypedef; symboltypedef]
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

let interface =
  { PreInterface.interface with
    typedecls = typedecls;
    valdecls = valdecls;
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

