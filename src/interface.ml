open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

include PreInterface

let typedefs =
  if Settings.typed_values then
    let nonterminaltypedef =
      let add_nt sym ocamltype datadefs =
        {
          dataname = "NT'" ^ Misc.normalize sym;
          datavalparams = [TypTextual ocamltype];
          datatypeparams = None;
        } :: datadefs
      in
      let datadefs =
        StringMap.fold add_nt
          Front.grammar.UnparameterizedSyntax.types
          []
      in
      {
        typename = "nonterminal";
        typeparams = [];
        typerhs = TDefSum datadefs;
        typeconstraint = None;
        typeprivate = false;
      }
    in
    let valuetypedef =
      {
        typename = "sem_value";
        typeparams = [];
        typerhs = TDefSum [
            {
              dataname = "Terminal";
              datavalparams = [TypTextual (Stretch.Inferred "token")];
              datatypeparams = None;
            };
            {
              dataname = "Nonterminal";
              datavalparams = [TypTextual (Stretch.Inferred "nonterminal")];
              datatypeparams = None;
            }
          ];
        typeconstraint = None;
        typeprivate = false;
      }
    in
    tokentypedef @ [nonterminaltypedef; valuetypedef]
  else
    tokentypedef

let interface =
  { PreInterface.interface with
    typedecls = typedefs; }

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

