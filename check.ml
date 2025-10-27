(** Type checking for session types
    
    This module provides type checking to verify that process implementations
    conform to their declared local session types.
    
    The implementation follows syntax-directed typing rules, with each case
    corresponding to a typing rule from session type theory.
*)

open Ast

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

exception TypeError of error

(** Convert error to string for display *)
let string_of_error = function
  | TypeMismatch (expected, actual) ->
      Printf.sprintf "Type mismatch: expected %s but got %s" expected actual
  | MissingBranch label ->
      Printf.sprintf "Missing branch for label '%s' in external choice" label
  | ExtraBranch label ->
      Printf.sprintf "Extra branch for label '%s' not in type" label
  | FreeVariable var ->
      Printf.sprintf "Unbound variable '%s'" var

(** Type environment mapping variables to base types *)
module Env = struct
  type t = (string * string) list
  
  let empty : t = []
  
  let add (var : string) (base_type : string) (env : t) : t =
    (var, base_type) :: env
  
  let lookup (var : string) (env : t) : string option =
    List.assoc_opt var env
end

(** Simple set for tracking bound process/type variables *)
module VarSet = struct
  type t = string list
  let empty = []
  let add x s = x :: s
  let mem x s = List.mem x s
end

(** Infer the base type of an expression with respect to a type environment
    
    Γ ⊢ e : T
    
    Returns the base type ("Int" or "Bool") of the expression.
*)
let rec infer_expr (env : Env.t) (e : 'v expr) : string =
  match e with
  | EInt _ -> "Int"
  | ETrue _ | EFalse _ -> "Bool"
  | EVar (x, _) ->
      begin match Env.lookup x env with
      | Some t -> t
      | None -> raise (TypeError (FreeVariable x))
      end
  | EPlus (e1, e2, _) | EMinus (e1, e2, _) | ETimes (e1, e2, _) 
  | EDiv (e1, e2, _) | EMod (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> "Int" then
        raise (TypeError (TypeMismatch ("Int", t1)));
      if t2 <> "Int" then
        raise (TypeError (TypeMismatch ("Int", t2)));
      "Int"
  | ELt (e1, e2, _) | EGt (e1, e2, _) | ELe (e1, e2, _) 
  | EGe (e1, e2, _) | EEq (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> "Int" then
        raise (TypeError (TypeMismatch ("Int", t1)));
      if t2 <> "Int" then
        raise (TypeError (TypeMismatch ("Int", t2)));
      "Bool"
  | EAnd (e1, e2, _) | EOr (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> "Bool" then
        raise (TypeError (TypeMismatch ("Bool", t1)));
      if t2 <> "Bool" then
        raise (TypeError (TypeMismatch ("Bool", t2)));
      "Bool"
  | ENot (e, _) ->
      let t = infer_expr env e in
      if t <> "Bool" then
        raise (TypeError (TypeMismatch ("Bool", t)));
      "Bool"
  | EChoice (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> t2 then
        raise (TypeError (TypeMismatch (t1, t2)));
      t1

(** Check that an expression has the expected base type
    
    Γ ⊢ e : T
*)
let check_expr (env : Env.t) (e : 'v expr) (expected : string) : unit =
  let actual = infer_expr env e in
  if actual <> expected then
    raise (TypeError (TypeMismatch (expected, actual)))

(** Main type checking function
    
    Syntax-directed typing rules:
    
    Γ; Δ ⊢ P ▷ T
    
    where:
      Γ = type environment (variable types)
      Δ = bound process/type variables
      P = process
      T = local session type
*)
let rec check 
    (env : Env.t)           (* Type environment *)
    (proc : string processes) 
    (typ : string local)
    (proc_vars : VarSet.t)  (* Bound process variables *)
    (type_vars : VarSet.t)  (* Bound type variables *)
    : unit =
  match proc, typ with
  
  (* ─────────────────────
     Γ; Δ ⊢ 0 ▷ end
  *)
  | PInact _, LEnd _ -> ()
  
  (* X ∈ Δ_proc
     ─────────────────────
     Γ; Δ ⊢ X ▷ T
  *)
  | PVar (x, _), _ ->
      if not (VarSet.mem x proc_vars) then
        raise (TypeError (FreeVariable x))
  
  (* Y ∈ Δ_type
     ─────────────────────
     Γ; Δ ⊢ P ▷ Y
  *)
  | _, LVar (y, _) ->
      if not (VarSet.mem y type_vars) then
        raise (TypeError (FreeVariable y))
  
  (* Γ; Δ, X ⊢ P ▷ T
     ─────────────────────────
     Γ; Δ ⊢ rec X.P ▷ rec Y.T
  *)
  | PRec (x, p, _), LRec (y, t, _) ->
      check env p t (VarSet.add x proc_vars) (VarSet.add y type_vars)
  
  (* Γ ⊢ e : B    Γ; Δ ⊢ P ▷ T
     ───────────────────────────────
     Γ; Δ ⊢ p!e.P ▷ p![B]; T
  *)
  | PSend (role, expr, cont, _), LSend (role_t, base_t, cont_t, _) ->
      if role <> role_t then
        raise (TypeError (TypeMismatch (
          Printf.sprintf "send to %s" role_t,
          Printf.sprintf "send to %s" role)));
      check_expr env expr base_t;
      check env cont cont_t proc_vars type_vars
  
  (* Γ, x:B; Δ ⊢ P ▷ T
     ───────────────────────────────
     Γ; Δ ⊢ p?(x).P ▷ p?[B]; T
  *)
  | PRecv (role, var, cont, _), LRecv (role_t, base_t, cont_t, _) ->
      if role <> role_t then
        raise (TypeError (TypeMismatch (
          Printf.sprintf "receive from %s" role_t,
          Printf.sprintf "receive from %s" role)));
      let env' = Env.add var base_t env in
      check env' cont cont_t proc_vars type_vars
  
  (* l ∈ {l₁,...,lₙ}    Γ; Δ ⊢ P ▷ Tₗ
     ────────────────────────────────────────────
     Γ; Δ ⊢ p:l.P ▷ p!{l₁:T₁, ..., lₙ:Tₙ}
  *)
  | PInt (role, label, cont, _), LInt (role_t, branches_t, _) ->
      if role <> role_t then
        raise (TypeError (TypeMismatch (
          Printf.sprintf "select label to %s" role_t,
          Printf.sprintf "select label to %s" role)));
      begin match List.assoc_opt label branches_t with
      | Some cont_t -> check env cont cont_t proc_vars type_vars
      | None -> 
          let available = String.concat ", " (List.map fst branches_t) in
          raise (TypeError (TypeMismatch (
            Printf.sprintf "label %s (available: %s)" label available,
            label)))
      end
  
  (* {l₁,...,lₙ} = labels(Γ; Δ ⊢ P)
     ∀i. Γ; Δ ⊢ Pᵢ ▷ Tᵢ
     ────────────────────────────────────────────
     Γ; Δ ⊢ p?{l₁:P₁,...,lₙ:Pₙ} ▷ p?{l₁:T₁,...,lₙ:Tₙ}
  *)
  | PExt (role, branches_p, _), LExt (role_t, branches_t, _) ->
      if role <> role_t then
        raise (TypeError (TypeMismatch (
          Printf.sprintf "offer choice to %s" role_t,
          Printf.sprintf "offer choice to %s" role)));
      
      (* Check all type branches are present in process *)
      List.iter (fun (label, _) ->
        if not (List.mem_assoc label branches_p) then
          raise (TypeError (MissingBranch label))
      ) branches_t;
      
      (* Check all process branches are in type *)
      List.iter (fun (label, _) ->
        if not (List.mem_assoc label branches_t) then
          raise (TypeError (ExtraBranch label))
      ) branches_p;
      
      (* Check each branch *)
      List.iter (fun (label, proc_cont) ->
        let type_cont = List.assoc label branches_t in
        check env proc_cont type_cont proc_vars type_vars
      ) branches_p
  
  (* Γ ⊢ e : Bool    Γ; Δ ⊢ P₁ ▷ T    Γ; Δ ⊢ P₂ ▷ T
     ───────────────────────────────────────────────────
     Γ; Δ ⊢ if e then P₁ else P₂ ▷ T
  *)
  | PIfThenElse (cond, then_p, else_p, _), _ ->
      check_expr env cond "Bool";
      check env then_p typ proc_vars type_vars;
      check env else_p typ proc_vars type_vars
  
  (* Structural mismatches *)
  | PInact _, _ ->
      raise (TypeError (TypeMismatch (
        Pretty.string_of_local typ,
        "terminated process (0)")))
  
  | _, LEnd _ ->
      raise (TypeError (TypeMismatch (
        "terminated process (end)",
        Pretty.string_of_process proc)))
  
  | PSend (role, _, _, _), _ ->
      raise (TypeError (TypeMismatch (
        Pretty.string_of_local typ,
        Printf.sprintf "send to %s" role)))
  
  | PRecv (role, _, _, _), _ ->
      raise (TypeError (TypeMismatch (
        Pretty.string_of_local typ,
        Printf.sprintf "receive from %s" role)))
  
  | PInt (role, label, _, _), _ ->
      raise (TypeError (TypeMismatch (
        Pretty.string_of_local typ,
        Printf.sprintf "select %s to %s" label role)))
  
  | PExt (role, _, _), _ ->
      raise (TypeError (TypeMismatch (
        Pretty.string_of_local typ,
        Printf.sprintf "offer choice to %s" role)))
  
  | PRec (x, _, _), _ ->
      raise (TypeError (TypeMismatch (
        Pretty.string_of_local typ,
        Printf.sprintf "recursive process (rec %s)" x)))

(** Public API: Check if a process conforms to a local type
    
    ∅; ∅ ⊢ P ▷ T
*)
let check_process (proc : string processes) (typ : string local) : unit =
  check Env.empty proc typ VarSet.empty VarSet.empty

(** Expression type inference (exposed for compatibility) *)
let infer_expr e = infer_expr Env.empty e

(** Expression type checking (exposed for compatibility) *)
let check_expr e expected = check_expr Env.empty e expected
