(* pretty.mli: Interface for pretty printing session types and processes *)

open Ast

(** Pretty print sorts (expression types) *)
val pp_sort : Format.formatter -> sort -> unit

(** Convert sort to string *)
val string_of_sort : sort -> string

(** Pretty print global types *)
val pp_global : Format.formatter -> string global -> unit

(** Pretty print local types *)
val pp_local : Format.formatter -> string local -> unit

(** Pretty print processes *)
val pp_process : Format.formatter -> string processes -> unit

(** Pretty print expressions *)
val pp_expr : Format.formatter -> string expr -> unit

(** Pretty print process definitions *)
val pp_process_definition : Format.formatter -> string process_definition -> unit

(** Pretty print tagged processes *)
val pp_tagged_process : Format.formatter -> string tagged_process -> unit

(** Pretty print programs *)
val pp_program : Format.formatter -> string program -> unit

(** Convert global type to string *)
val string_of_global : string global -> string

(** Convert local type to string *)
val string_of_local : string local -> string

(** Convert process to string *)
val string_of_process : string processes -> string

(** Convert expression to string *)
val string_of_expr : string expr -> string

(** Convert program to string *)
val string_of_program : string program -> string

