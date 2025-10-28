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

(** Expression variable environment: maps variables to sorts *)
module ExprEnv = struct
  type t = (string * sort) list
  
  let empty : t = []
  
  let add (var : string) (s : sort) (env : t) : t =
    (var, s) :: env
  
  let lookup (var : string) (env : t) : sort option =
    List.assoc_opt var env
end

(** Process variable environment: maps process variables to local types *)
module ProcEnv = struct
  type t = (string * string local) list
  
  let empty : t = []
  
  let add (var : string) (typ : string local) (env : t) : t =
    (var, typ) :: env
  
  let lookup (var : string) (env : t) : string local option =
    List.assoc_opt var env
end

(** Infer the sort of an expression with respect to a type environment
    
    Γ ⊢ e : S
    
    Returns the sort (SInt or SBool) of the expression.
*)
let rec infer_expr (env : ExprEnv.t) (e : 'v expr) : sort =
  match e with
  | EInt _ -> SInt
  | ETrue _ | EFalse _ -> SBool
  | EVar (x, _) ->
      begin match ExprEnv.lookup x env with
      | Some s -> s
      | None -> raise (TypeError (FreeVariable x))
      end
  | EPlus (e1, e2, _) | EMinus (e1, e2, _) | ETimes (e1, e2, _) 
  | EDiv (e1, e2, _) | EMod (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> SInt then
        raise (TypeError (TypeMismatch ("Int", Pretty.string_of_sort t1)));
      if t2 <> SInt then
        raise (TypeError (TypeMismatch ("Int", Pretty.string_of_sort t2)));
      SInt
  | ELt (e1, e2, _) | EGt (e1, e2, _) | ELe (e1, e2, _) 
  | EGe (e1, e2, _) | EEq (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> SInt then
        raise (TypeError (TypeMismatch ("Int", Pretty.string_of_sort t1)));
      if t2 <> SInt then
        raise (TypeError (TypeMismatch ("Int", Pretty.string_of_sort t2)));
      SBool
  | EAnd (e1, e2, _) | EOr (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> SBool then
        raise (TypeError (TypeMismatch ("Bool", Pretty.string_of_sort t1)));
      if t2 <> SBool then
        raise (TypeError (TypeMismatch ("Bool", Pretty.string_of_sort t2)));
      SBool
  | ENot (e, _) ->
      let t = infer_expr env e in
      if t <> SBool then
        raise (TypeError (TypeMismatch ("Bool", Pretty.string_of_sort t)));
      SBool
  | EChoice (e1, e2, _) ->
      let t1 = infer_expr env e1 in
      let t2 = infer_expr env e2 in
      if t1 <> t2 then
        raise (TypeError (TypeMismatch (Pretty.string_of_sort t1, Pretty.string_of_sort t2)));
      t1

(** Check that an expression has the expected base type
    
    Γ ⊢ e : S
    
    The expected type is given as a base string ("Int" or "Bool") to match
    the AST representation in local types.
*)
let check_expr (env : ExprEnv.t) (e : 'v expr) (expected : base) : unit =
  let expected_sort = match expected with
    | "Int" -> SInt
    | "Bool" -> SBool
    | _ -> raise (TypeError (TypeMismatch (expected, "unknown base type")))
  in
  let actual = infer_expr env e in
  if actual <> expected_sort then
    raise (TypeError (TypeMismatch (expected, Pretty.string_of_sort actual)))

(** Main type checking function
    
    Syntax-directed typing rules:
    
    Γ; Δ ⊢ P ▷ T
    
    where:
      Γ = expression variable environment (string -> sort)
      Δ = process variable environment (string -> local)
      P = process
      T = local session type
*)
let rec check 
    (expr_env : ExprEnv.t)
    (proc_env : ProcEnv.t)
    (proc : string processes) 
    (typ : string local)
    : unit =
  match proc, typ with
  (* Γ; Δ ⊢ P ▷ unfold_local T
     ─────────────────────
     Γ; Δ ⊢ P ▷ T
  *)
  | _, LRec (_y, _t, _) -> 
    let typ' = Utils.unfold_local_once typ in
    check expr_env proc_env proc typ'

  (* Γ; Δ, X : T ⊢ P ▷ T
     ─────────────────────────
     Γ; Δ ⊢ rec X . P ▷ T
  *)
  | PRec (x, p, _), _ ->
    let proc_env' = ProcEnv.add x typ proc_env in
    check expr_env proc_env' p typ 

  (* ─────────────────────
     Γ; Δ ⊢ 0 ▷ end
  *)
  | PInact _, LEnd _ -> ()
  
  (* X ∈ Δ_proc    Δ(X) <= T
     ─────────────────────────
     Γ; Δ ⊢ X ▷ T
  *)
  | PVar (x, _), _ ->
    begin match ProcEnv.lookup x proc_env with
    | Some expected_typ -> 
      (* Check that the expected type is a subtype of the required type *)
      if not (Subtype.is_subtype expected_typ typ) then
        raise (TypeError (TypeMismatch (
          Pretty.string_of_local typ,
          Printf.sprintf "%s (has type %s)" x (Pretty.string_of_local expected_typ))))
    | None -> raise (TypeError (FreeVariable x))
    end
    
  (* Γ ⊢ e : B    Γ; Δ ⊢ P ▷ T
     ───────────────────────────────
     Γ; Δ ⊢ p!e.P ▷ p![B]; T
  *)
  | PSend (role, expr, cont, _), LSend (role_t, base_t, cont_t, _) ->
      if role <> role_t then
        raise (TypeError (TypeMismatch (
          Printf.sprintf "send to %s" role_t,
          Printf.sprintf "send to %s" role)));
      check_expr expr_env expr base_t;
      check expr_env proc_env cont cont_t
  
  (* Γ, x:B; Δ ⊢ P ▷ T
     ───────────────────────────────
     Γ; Δ ⊢ p?(x).P ▷ p?[B]; T
  *)
  | PRecv (role, var, cont, _), LRecv (role_t, base_t, cont_t, _) ->
      if role <> role_t then
        raise (TypeError (TypeMismatch (
          Printf.sprintf "receive from %s" role_t,
          Printf.sprintf "receive from %s" role)));
      (* Convert base type string to sort *)
      let sort = match base_t with
        | "Int" -> SInt
        | "Bool" -> SBool
        | _ -> raise (TypeError (TypeMismatch (base_t, "unknown base type")))
      in
      let expr_env' = ExprEnv.add var sort expr_env in
      check expr_env' proc_env cont cont_t
  
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
      | Some cont_t -> check expr_env proc_env cont cont_t
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
        check expr_env proc_env proc_cont type_cont
      ) branches_p
  
  (* Γ ⊢ e : Bool    Γ; Δ ⊢ P₁ ▷ T    Γ; Δ ⊢ P₂ ▷ T
     ───────────────────────────────────────────────────
     Γ; Δ ⊢ if e then P₁ else P₂ ▷ T
  *)
  | PIfThenElse (cond, then_p, else_p, _), _ ->
      check_expr expr_env cond "Bool";
      check expr_env proc_env then_p typ;
      check expr_env proc_env else_p typ
  
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

(** Public API: Check if a process conforms to a local type
    
    ∅; ∅ ⊢ P ▷ T
*)
let check_process (proc : string processes) (typ : string local) : unit =
  check ExprEnv.empty ProcEnv.empty proc typ

(** Expression type inference (exposed for compatibility) *)
let infer_expr e = infer_expr ExprEnv.empty e

(** Expression type checking (exposed for compatibility) *)
let check_expr e expected = check_expr ExprEnv.empty e expected
