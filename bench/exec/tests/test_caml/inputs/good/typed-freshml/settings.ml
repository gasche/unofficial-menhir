(* Prepare for parsing the command line. *)

let verbose =
  ref 0

let filename = 
  ref None

let insert name = 
  filename := Some name

let options = Arg.align [
  "-v", Arg.Set_int verbose, "<level> Sets verbosity level";
]

let usage =
  Printf.sprintf "Usage: %s <options> <filename>" Sys.argv.(0)

(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* Export the settings. *)

let verbose =
  !verbose

let filename =
  match !filename with
  | None ->
      Arg.usage options usage;
      exit 1
  | Some filename ->
      filename

