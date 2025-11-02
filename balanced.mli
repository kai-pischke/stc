(* balanced.mli: Interface for balancedness checking *)

(** Exception raised when a global type is unbalanced *)
exception Unbalanced of string

(** Check if a global type is balanced.
    
    A global type is balanced if at every branching point (GBra), 
    the set of unavoidable participants equals the set of reachable participants.
    
    - Reachable: roles that appear in at least one branch
    - Unavoidable: roles that appear in all branches
    
    @param g The global type to check
    @return (is_balanced, error_message) where error_message is Some msg if unbalanced *)
val check_balanced : string Ast.global -> (bool * string option)

