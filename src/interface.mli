(* This module defines the interface of the generated parser. *)

(* This is the [Error] exception. *)

val excname: string
val excdef: IL.excdef
val excredef: IL.excdef

(* The type of the entry point for the nonterminal start symbol
   [symbol]. *)

val entrytypescheme: string -> IL.typescheme

(* All type definitions that should be included in generated implementation
   Include [TokenType.tokentypedef] and type for non-terminals and semantic
   values, if requested by user. *)

val typedefs: IL.typedef list

(* This writes the interface of the generated parser to the [.mli]
   file. *)

val write: unit -> unit

