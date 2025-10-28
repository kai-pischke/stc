(** Utility functions for session type manipulation
    
    This module provides common utilities for working with session types,
    including substitution, unfolding, and other transformations.
*)

open Ast

(** Substitute a type variable with a replacement type in a local type
    
    subst_local x T_replacement T = T[T_replacement/x]
    
    Replaces all free occurrences of variable x with T_replacement in T.
    Bound variables (under rec) are not substituted.
*)
let rec subst_local (var : string) (replacement : string local) (typ : string local) : string local =
  match typ with
  | LEnd loc -> LEnd loc
  | LVar (x, loc) ->
      if x = var then replacement else LVar (x, loc)
  | LRec (x, body, loc) ->
      if x = var then
        (* Variable is bound here, don't substitute inside *)
        LRec (x, body, loc)
      else
        (* Variable is not bound, substitute in body *)
        LRec (x, subst_local var replacement body, loc)
  | LSend (role, base, cont, loc) ->
      LSend (role, base, subst_local var replacement cont, loc)
  | LRecv (role, base, cont, loc) ->
      LRecv (role, base, subst_local var replacement cont, loc)
  | LInt (role, branches, loc) ->
      let branches' = List.map (fun (label, t) ->
        (label, subst_local var replacement t)
      ) branches in
      LInt (role, branches', loc)
  | LExt (role, branches, loc) ->
      let branches' = List.map (fun (label, t) ->
        (label, subst_local var replacement t)
      ) branches in
      LExt (role, branches', loc)

(** Unfold a recursive type once
    
    unfold_local_once (rec t.T) = T[rec t.T / t]
    
    For a recursive type rec t.T, returns T with all occurrences of t
    replaced by rec t.T. For non-recursive types, returns the type unchanged.
    
    This is useful for checking that recursive processes match recursive types,
    as it allows comparing the body of the recursion after one unfolding.
*)
let unfold_local_once (typ : string local) : string local =
  match typ with
  | LRec (var, body, _loc) ->
      (* Replace all occurrences of var in body with the full recursive type *)
      subst_local var typ body
  | _ ->
      (* Not a recursive type, return unchanged *)
      typ

(** Unfold a recursive type completely until a non-binder is reached
    
    unfold_local T repeatedly applies unfold_local_once until the result
    is no longer a recursive type (rec binder).
    
    Examples:
    - unfold_local (rec T.rec S.p![Int];S) = p![Int]; rec S.p![Int]; S
    - unfold_local (rec T.p![Int];T) = p![Int];rec T.p![Int];T
    - unfold_local (p![Int];end) = p![Int];end (unchanged)
    
    This is useful for stripping outer recursive binders to expose the
    underlying structure of the type.
*)
let rec unfold_local (typ : string local) : string local =
  match typ with
  | LRec _ ->
      (* Unfold once and continue *)
      let unfolded = unfold_local_once typ in
      unfold_local unfolded
  | _ ->
      (* Not a recursive type, we're done *)
      typ

(** Check if a type is a type variable *)
let is_type_var (typ : string local) : bool =
  match typ with
  | LVar _ -> true
  | _ -> false

(** Check if a type is recursive *)
let is_recursive_type (typ : string local) : bool =
  match typ with
  | LRec _ -> true
  | _ -> false

(** Get the variable name from a type variable (if it is one) *)
let get_type_var_name (typ : string local) : string option =
  match typ with
  | LVar (x, _) -> Some x
  | _ -> None

(** Get the recursion variable and body from a recursive type (if it is one) *)
let get_rec_parts (typ : string local) : (string * string local) option =
  match typ with
  | LRec (var, body, _) -> Some (var, body)
  | _ -> None

(** Substitute a process variable with a replacement process
    
    subst_process_var x P_replacement P = P[P_replacement/x]
    
    Replaces all free occurrences of variable x with P_replacement in P.
    Bound variables (under rec) are not substituted.
*)
let rec subst_process_var (var : string) (replacement : string processes) (proc : string processes) : string processes =
  match proc with
  | PInact loc -> PInact loc
  | PVar (x, loc) ->
      if x = var then replacement else PVar (x, loc)
  | PRec (x, body, loc) ->
      if x = var then
        (* Variable is bound here, don't substitute inside *)
        PRec (x, body, loc)
      else
        (* Variable is not bound, substitute in body *)
        PRec (x, subst_process_var var replacement body, loc)
  | PSend (role, expr, cont, loc) ->
      PSend (role, expr, subst_process_var var replacement cont, loc)
  | PRecv (role, v, cont, loc) ->
      PRecv (role, v, subst_process_var var replacement cont, loc)
  | PInt (role, label, cont, loc) ->
      PInt (role, label, subst_process_var var replacement cont, loc)
  | PExt (role, branches, loc) ->
      let branches' = List.map (fun (label, p) ->
        (label, subst_process_var var replacement p)
      ) branches in
      PExt (role, branches', loc)
  | PIfThenElse (cond, then_p, else_p, loc) ->
      PIfThenElse (cond,
                   subst_process_var var replacement then_p,
                   subst_process_var var replacement else_p,
                   loc)

(** Unfold a recursive process once
    
    unfold_process_once (rec X.P) = P[rec X.P / X]
    
    For a recursive process rec X.P, returns P with all occurrences of X
    replaced by rec X.P. For non-recursive processes, returns the process unchanged.
*)
let unfold_process_once (proc : string processes) : string processes =
  match proc with
  | PRec (var, body, _loc) ->
      subst_process_var var proc body
  | _ ->
      proc

(** Check if a process is a process variable *)
let is_process_var (proc : string processes) : bool =
  match proc with
  | PVar _ -> true
  | _ -> false

(** Check if a process is recursive *)
let is_recursive_process (proc : string processes) : bool =
  match proc with
  | PRec _ -> true
  | _ -> false

(** Get the variable name from a process variable (if it is one) *)
let get_process_var_name (proc : string processes) : string option =
  match proc with
  | PVar (x, _) -> Some x
  | _ -> None

(** Get the recursion variable and body from a recursive process (if it is one) *)
let get_rec_process_parts (proc : string processes) : (string * string processes) option =
  match proc with
  | PRec (var, body, _) -> Some (var, body)
  | _ -> None

(** {1 De Bruijn Representation} *)

(** Convert a local type from named variables to De Bruijn indices
    
    Variables are represented as indices indicating how many binders away
    the binding rec is. For example:
    - rec t. p![Int]; t  becomes  rec 0. p![Int]; 0
    - rec t. rec s. p![Int]; s  becomes  rec 0. rec 0. p![Int]; 0
    
    IMPORTANT: Branch lists are normalized by sorting labels alphabetically.
    This ensures that semantically equivalent types have identical De Bruijn
    representations, which is used for efficient equality checking.
    
    @param typ The type with named variables
    @return The type with De Bruijn indices (branches sorted by label)
*)
let to_debruijn (typ : string local) : int local =
  let rec convert (env : (string * int) list) (depth : int) (t : string local) : int local =
    match t with
    | LEnd loc -> LEnd loc
    | LVar (name, loc) ->
        (* Look up the variable in the environment *)
        begin match List.assoc_opt name env with
        | Some binding_depth -> LVar (depth - binding_depth - 1, loc)
        | None -> failwith (Printf.sprintf "Unbound type variable: %s" name)
        end
    | LRec (var, body, loc) ->
        (* Add this variable at current depth, increment depth for body *)
        let env' = (var, depth) :: env in
        LRec (depth, convert env' (depth + 1) body, loc)
    | LSend (role, base, cont, loc) ->
        LSend (role, base, convert env depth cont, loc)
    | LRecv (role, base, cont, loc) ->
        LRecv (role, base, convert env depth cont, loc)
    | LInt (role, branches, loc) ->
        (* Convert and sort branches by label *)
        let branches' = List.map (fun (label, t) ->
          (label, convert env depth t)
        ) branches in
        let branches_sorted = List.sort (fun (l1, _) (l2, _) -> String.compare l1 l2) branches' in
        LInt (role, branches_sorted, loc)
    | LExt (role, branches, loc) ->
        (* Convert and sort branches by label *)
        let branches' = List.map (fun (label, t) ->
          (label, convert env depth t)
        ) branches in
        let branches_sorted = List.sort (fun (l1, _) (l2, _) -> String.compare l1 l2) branches' in
        LExt (role, branches_sorted, loc)
  in
  convert [] 0 typ

(** Convert a local type from De Bruijn indices back to named variables
    
    This is mainly useful for pretty-printing or debugging.
    Generated names are t0, t1, t2, etc.
    
    @param typ The type with De Bruijn indices
    @return The type with generated variable names
*)
let from_debruijn (typ : int local) : string local =
  let rec convert (env : string list) (t : int local) : string local =
    match t with
    | LEnd loc -> LEnd loc
    | LVar (idx, loc) ->
        (* idx is the De Bruijn index, env has the names *)
        if idx < List.length env then
          LVar (List.nth env idx, loc)
        else
          failwith (Printf.sprintf "Unbound De Bruijn index: %d" idx)
    | LRec (_, body, loc) ->
        (* Generate a fresh name *)
        let var_name = Printf.sprintf "t%d" (List.length env) in
        (* Add to environment at position 0 (shift others) *)
        let env' = var_name :: env in
        LRec (var_name, convert env' body, loc)
    | LSend (role, base, cont, loc) ->
        LSend (role, base, convert env cont, loc)
    | LRecv (role, base, cont, loc) ->
        LRecv (role, base, convert env cont, loc)
    | LInt (role, branches, loc) ->
        let branches' = List.map (fun (label, t) ->
          (label, convert env t)
        ) branches in
        LInt (role, branches', loc)
    | LExt (role, branches, loc) ->
        let branches' = List.map (fun (label, t) ->
          (label, convert env t)
        ) branches in
        LExt (role, branches', loc)
  in
  convert [] typ

(** Unfold a De Bruijn recursive type once
    
    For a type rec n.T, this substitutes all occurrences of variable n
    with the full recursive type (rec n.T).
    
    IMPORTANT: Maintains sorted branch invariant.
    
    @param typ The De Bruijn type to unfold
    @return The unfolded type (with branches still sorted)
*)
let unfold_debruijn_once (typ : int local) : int local =
  match typ with
  | LRec (_, body, _) ->
      (* Substitute index 0 (the bound variable) with the full rec type *)
      let rec subst (depth : int) (t : int local) : int local =
        match t with
        | LEnd loc -> LEnd loc
        | LVar (idx, loc) ->
            (* If this variable refers to the binder we're unfolding *)
            if idx = depth then typ
            else LVar (idx, loc)
        | LRec (n, body, loc) ->
            (* When entering a binder, increment depth *)
            LRec (n, subst (depth + 1) body, loc)
        | LSend (role, base, cont, loc) ->
            LSend (role, base, subst depth cont, loc)
        | LRecv (role, base, cont, loc) ->
            LRecv (role, base, subst depth cont, loc)
        | LInt (role, branches, loc) ->
            (* Branches are already sorted, maintain that invariant *)
            let branches' = List.map (fun (label, t) ->
              (label, subst depth t)
            ) branches in
            LInt (role, branches', loc)
        | LExt (role, branches, loc) ->
            (* Branches are already sorted, maintain that invariant *)
            let branches' = List.map (fun (label, t) ->
              (label, subst depth t)
            ) branches in
            LExt (role, branches', loc)
      in
      subst 0 body
  | _ -> typ

