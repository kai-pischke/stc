(* interpreter.mli: Runtime interpreter for programs *)

open Ast

(** Runtime values *)
type value =
  | VInt of int
  | VBool of bool

(** Runtime errors *)
type error =
  | TypeError of string          (* Type mismatch *)
  | UndefinedVariable of string  (* Variable not in scope *)
  | UndefinedProcess of string   (* Process definition not found *)
  | NoSuchParticipant of string  (* Participant doesn't exist *)
  | CommunicationMismatch of string (* Sender/receiver mismatch *)
  | StuckProcess of string       (* Process cannot make progress *)
  | InvalidChoice of string      (* Choice label not available *)

exception RuntimeError of error

(** Format error for display *)
val string_of_error : error -> string

(** String representation of values *)
val string_of_value : value -> string

(** Execute a program
    Returns the final state or raises RuntimeError *)
val execute_program : string program -> unit

