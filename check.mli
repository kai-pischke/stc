(** Type checking for session types
    
    This module provides type checking to verify that process implementations
    conform to their declared local session types.
    
    Note: This is type CHECKING, not type INFERENCE. We verify that a process
    matches a given type, but we don't infer types from unannotated processes.
*)

(** Type checking errors *)
type error =
  | TypeMismatch of string * string
      (** Expected vs. actual structure mismatch *)
  | MissingBranch of string
      (** External choice missing a required branch *)
  | ExtraBranch of string
      (** External choice has an unexpected branch *)
  | FreeVariable of string
      (** Unbound variable in process or type *)

(** Exception raised when type checking fails *)
exception TypeError of error

(** Convert error to human-readable string *)
val string_of_error : error -> string

(** Infer the sort (type) of an expression
    
    Returns SInt for integer expressions and SBool for boolean expressions.
    Variables are looked up in the type environment.
    
    Note: This version uses an empty type environment. For checking with
    a specific environment, use the internal functions.
    
    @param e The expression to infer the type of
    @return The sort (SInt or SBool)
    @raise TypeError if expression contains free variables
*)
val infer_expr : 'v Ast.expr -> Ast.sort

(** Check if an expression has a given base type
    
    Raises TypeError if the expression doesn't match the expected type.
    
    @param e The expression to check
    @param expected The expected base type ("Int" or "Bool")
    @raise TypeError if the types don't match
*)
val check_expr : 'v Ast.expr -> Ast.base -> unit

(** Check if a process conforms to a local session type
    
    This is the main entry point for type checking. It implements syntax-directed
    typing rules for session types, verifying that:
    - Communication actions match (send/receive with correct roles)
    - Internal/external choices have matching labels
    - Base types of sent expressions match the type specification
    - Received variables are properly typed in continuations
    - Process structure matches type structure (recursion, termination)
    
    The checker maintains a type environment to track variable types through
    receive operations, enabling proper type checking of expressions involving
    received values.
    
    @param proc The process to check
    @param typ The local type to check against
    @raise TypeError if the process doesn't conform to the type
    
    Example:
    {[
      let proc = Parse.process_from_string "server![42].0" in
      let typ = Parse.local_from_string "server![Int];end" in
      check_process proc typ  (* OK *)
      
      let proc2 = Parse.process_from_string "server![true].0" in
      check_process proc2 typ  (* Raises TypeError - Bool vs Int *)
      
      (* Variables are now tracked properly *)
      let proc3 = Parse.process_from_string "s?(x).t![x].0" in
      let typ3 = Parse.local_from_string "s?[Bool];t![Int];end" in
      check_process proc3 typ3  (* Raises TypeError - x is Bool, not Int *)
    ]}
*)
val check_process : string Ast.processes -> string Ast.local -> unit
