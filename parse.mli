(* parse.mli: Interface for parsing session types with error handling *)

open Ast

(** Parse error with location information *)
type error = {
  message : string;
  line : int;
  column : int;
  position : int;
}

(** Exception raised on parse errors *)
exception ParseError of error

(** Format error for display *)
val string_of_error : error -> string

(** Parse global type from string *)
val global_from_string : string -> string global

(** Parse local type from string *)
val local_from_string : string -> string local

(** Parse process from string *)
val process_from_string : string -> string processes

(** Parse program from string *)
val program_from_string : string -> string program

(** Parse global type from file *)
val global_from_file : string -> string global

(** Parse local type from file *)
val local_from_file : string -> string local

(** Parse process from file *)
val process_from_file : string -> string processes

(** Parse program from file *)
val program_from_file : string -> string program

