(* wellformed.mli: Well-formedness checking for session types *)

open Ast

(** Well-formedness errors *)
type error =
  | FreeVariable of string * string  (* variable name, context *)
  | UnguardedRecursion of string     (* variable name *)
  | SelfCommunication of string      (* role name *)
  | EmptyChoice of string            (* context *)
  | DuplicateLabel of string * string  (* label name, context *)
  | OpenParallel of string           (* variable name that's free in parallel side *)

(** Exception raised when well-formedness check fails *)
exception IllFormed of error

(** Format error for display *)
val string_of_error : error -> string

(** Check if a global type is well-formed 
    Checks:
    - No free variables
    - No unguarded recursion
    - No self-communication (p -> p)
    - Choice branches are non-empty
    - No duplicate labels in choices
    - Both sides of parallel composition must be closed (no free variables)
*)
val check_global : string global -> unit

(** Check if a local type is well-formed
    Checks:
    - No free variables
    - No unguarded recursion
    - Choice branches are non-empty
    - No duplicate labels in choices
*)
val check_local : string local -> unit

(** Check if a process is well-formed
    Checks:
    - No free process variables
    - No unguarded recursion
    - Choice branches are non-empty
    - No duplicate labels in choices
    - Expression variables are not checked (assumed bound elsewhere)
*)
val check_process : string processes -> unit

