open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

include PreInterface

let interface =
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
      let datadefs = StringMap.fold add_n Front.grammar.types [] in
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
    {
      PreInterface.interface with

      typedecls = tokentypedef @ [symbolclasstypedef; symboltypedef];
    }
  else
    PreInterface.interface

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

