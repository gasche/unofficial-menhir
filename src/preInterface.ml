(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  Fran�ois Pottier, INRIA Rocquencourt                                  *)
(*  Yann R�gis-Gianas, PPS, Universit� Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

(* This is the [Error] exception. *)

let excname =
  "Error"

let excdef = {
  excname = excname;
  excrhs = (if Settings.fixedexc
            then ExcRebind "Parsing.Parse_error"
            else ExcDecl []);
}

let excredef = {
  excdef with excrhs = ExcRebind excname
}

(* The type of the entry point for the start symbol [symbol]. *)

let entrytypescheme symbol =
  let ocamltype =
    try
      StringMap.find symbol PreFront.grammar.types
    with Not_found ->
      (* Every start symbol should have a type. *)
      assert false
  in
  type2scheme (marrow [ arrow tlexbuf ttoken; tlexbuf ] (TypTextual ocamltype))

let entryvaldecls =
  StringSet.fold (fun symbol decls ->
      (Misc.normalize symbol, entrytypescheme symbol) :: decls
    ) PreFront.grammar.start_symbols []

(* Type definitions of stepwise interface *)

let steptypedef =
  let tyenv = TypApp ("MenhirLib.EngineTypes.env", [
      TypApp ("state",[]); TypApp ("semantic_value",[]); TypApp ("token",[]);
    ])
  in
  [
    { typename = "state"; typerhs = TDefSum [];
      typeparams = []; typeconstraint = None; typeprivate = false };
    { typename = "semantic_value";
      typerhs = TAbbrev (TypApp ("Obj.t", []));
      typeparams = []; typeconstraint = None; typeprivate = false };
    { typename = "step"; typeprivate = true;
      typerhs = TDefSum [
          { dataname = "Step_run";
            datavalparams = [tyenv];
            datatypeparams = None
          };
          { dataname = "Step_error";
            datavalparams = [tyenv];
            datatypeparams = None
          };
          { dataname = "Step_action";
            datavalparams = [tyenv];
            datatypeparams = None
          };
        ];
      typeparams = []; typeconstraint = None };
    { typename = "parser"; typeprivate = false;
      typerhs = TDefSum [
          { dataname = "Step";
            datavalparams = [TypApp ("step",[])];
            datatypeparams = None
          };
          { dataname = "Accept";
            datavalparams = [TypApp ("semantic_value",[])];
            datatypeparams = None
          };
          { dataname = "Reject";
            datavalparams = [];
            datatypeparams = None
          };
          { dataname = "Feed";
            datavalparams = [TypArrow (TypTuple [
                TypApp ("Lexing.position",[]);
                TypApp ("token",[]);
                TypApp ("Lexing.position",[]);
              ], TypApp ("step",[]))];
            datatypeparams = None
          };
        ];
      typeparams = []; typeconstraint = None };
  ]

let stepvaldecls = [
  "initial", { quantifiers = [];
               body = TypArrow (TypApp ("state",[]), TypApp ("parser",[])) };
  "step", { quantifiers = [];
            body = TypArrow (TypApp ("step",[]), TypApp ("parser",[])) };
]

let stepentryvaldecls =
  StringSet.fold (fun symbol decls ->
      (Misc.normalize symbol ^ "_state",
       {quantifiers = []; body = TypApp ("state",[])}) :: decls
    ) PreFront.grammar.start_symbols []


(* This is the interface of the generated parser. *)

let interface = {

  paramdecls =
    PreFront.grammar.parameters;

  excdecls =
    [ excdef ];

  typedecls =
    tokentypedef @
      (if Settings.stepwise then steptypedef else []);

  valdecls =
    entryvaldecls @
      (if Settings.stepwise then stepvaldecls @ stepentryvaldecls else []);

  moddecls =
    [];

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

