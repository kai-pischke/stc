(** Subtyping for session types
    
    This module implements the Gay and Hole (2005) coinductive subtyping algorithm
    for session types with recursion. The algorithm handles recursive types by
    assuming subtyping obligations while checking their unfolded bodies.
    
    Key subtyping rules:
    - end <= end
    - p![B]; T1 <= p![B]; T2  if T1 <= T2
    - p?[B]; T1 <= p?[B]; T2  if T1 <= T2
    - Internal choice is covariant: more options is a subtype
    - External choice is contravariant: fewer options is a subtype
*)

(** Exception raised when subtyping check fails *)
exception SubtypeError of string

(** Check if t1 is a subtype of t2
    
    Returns true if t1 <= t2, false otherwise.
    
    Examples of valid subtypes:
    - end <= end
    - p![Int]; end <= p![Int]; end
    - p!{l1: end, l2: end} <= p!{l1: end, l2: end, l3: end} (covariant)
    - p?{l1: end, l2: end, l3: end} <= p?{l1: end, l2: end} (contravariant)
    - rec t. p![Int]; t <= rec t. p![Int]; t
    
    @param t1 The subtype candidate
    @param t2 The supertype candidate
    @return true if t1 is a subtype of t2
*)
val is_subtype : string Ast.local -> string Ast.local -> bool

(** Check subtyping and raise an exception if it fails
    
    @param t1 The subtype candidate
    @param t2 The supertype candidate
    @raise SubtypeError if t1 is not a subtype of t2
*)
val check_subtype : string Ast.local -> string Ast.local -> unit

