(** Utility functions for session type manipulation *)

(** Substitute a type variable with a replacement type *)
val subst_local : string -> string Ast.local -> string Ast.local -> string Ast.local

(** Unfold a recursive type once *)
val unfold_local_once : string Ast.local -> string Ast.local

(** Unfold a recursive type completely until a non-binder is reached *)
val unfold_local : string Ast.local -> string Ast.local

(** Check if a type is a type variable *)
val is_type_var : string Ast.local -> bool

(** Check if a type is recursive *)
val is_recursive_type : string Ast.local -> bool

(** Get the variable name from a type variable *)
val get_type_var_name : string Ast.local -> string option

(** Get the recursion variable and body from a recursive type *)
val get_rec_parts : string Ast.local -> (string * string Ast.local) option

(** {1 Process Substitution and Unfolding} *)

(** Substitute a process variable with a replacement process *)
val subst_process_var : string -> string Ast.processes -> string Ast.processes -> string Ast.processes

(** Unfold a recursive process once *)
val unfold_process_once : string Ast.processes -> string Ast.processes

(** Check if a process is a process variable *)
val is_process_var : string Ast.processes -> bool

(** Check if a process is recursive *)
val is_recursive_process : string Ast.processes -> bool

(** Get the variable name from a process variable *)
val get_process_var_name : string Ast.processes -> string option

(** Get the recursion variable and body from a recursive process *)
val get_rec_process_parts : string Ast.processes -> (string * string Ast.processes) option

