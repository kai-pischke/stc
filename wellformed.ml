(* wellformed.ml: Well-formedness checking for session types *)

open Ast

(** Well-formedness errors *)
type error =
  | FreeVariable of string * string
  | UnguardedRecursion of string
  | SelfCommunication of string
  | EmptyChoice of string
  | DuplicateLabel of string * string
  | OpenParallel of string  (* variable name that's free in parallel side *)

exception IllFormed of error

(** Format error for display *)
let string_of_error = function
  | FreeVariable (var, ctx) ->
      Printf.sprintf "Free variable '%s' in %s" var ctx
  | UnguardedRecursion var ->
      Printf.sprintf "Unguarded recursion for variable '%s'" var
  | SelfCommunication role ->
      Printf.sprintf "Self-communication: role '%s' sends to itself" role
  | EmptyChoice ctx ->
      Printf.sprintf "Empty choice in %s" ctx
  | DuplicateLabel (label, ctx) ->
      Printf.sprintf "Duplicate label '%s' in %s" label ctx
  | OpenParallel var ->
      Printf.sprintf "Parallel composition has open side with free variable '%s' (both sides must be closed)" var

(** Set operations for variable tracking *)
module StringSet = Set.Make(String)

(** Check for duplicate labels in a branch list *)
let check_unique_labels (branches : (string * 'a) list) (context : string) : unit =
  let rec check_dups seen = function
    | [] -> ()
    | (label, _) :: rest ->
        if StringSet.mem label seen then
          raise (IllFormed (DuplicateLabel (label, context)))
        else
          check_dups (StringSet.add label seen) rest
  in
  check_dups StringSet.empty branches

(** Get free variables in a global type *)
let rec get_free_global (bound : StringSet.t) (g : string global) : StringSet.t =
  match g with
  | GEnd _ -> StringSet.empty
  | GVar (v, _) ->
      if StringSet.mem v bound then StringSet.empty
      else StringSet.singleton v
  | GRec (v, body, _) ->
      get_free_global (StringSet.add v bound) body
  | GMsg (_, _, _, cont, _) ->
      get_free_global bound cont
  | GBra (_, _, branches, _) ->
      List.fold_left (fun acc (_, g) ->
        StringSet.union acc (get_free_global bound g)
      ) StringSet.empty branches
  | GPar (g1, g2, _) ->
      StringSet.union (get_free_global bound g1) (get_free_global bound g2)

(** Check for free variables in global types *)
let rec check_global_free (bound : StringSet.t) (g : string global) : unit =
  match g with
  | GEnd _ -> ()
  | GVar (v, _) ->
      if not (StringSet.mem v bound) then
        raise (IllFormed (FreeVariable (v, "global type")))
  | GRec (v, body, _) ->
      check_global_free (StringSet.add v bound) body
  | GMsg (p, q, _, cont, _) ->
      if p = q then
        raise (IllFormed (SelfCommunication p));
      check_global_free bound cont
  | GBra (p, q, branches, _) ->
      if p = q then
        raise (IllFormed (SelfCommunication p));
      if branches = [] then
        raise (IllFormed (EmptyChoice "global branching"));
      check_unique_labels branches "global branching";
      List.iter (fun (_, g) -> check_global_free bound g) branches
  | GPar (g1, g2, _) ->
      (* Check that both sides are closed (no free variables independently) *)
      let free1 = get_free_global StringSet.empty g1 in
      if not (StringSet.is_empty free1) then (
        let var = StringSet.choose free1 in
        raise (IllFormed (OpenParallel var))
      );
      let free2 = get_free_global StringSet.empty g2 in
      if not (StringSet.is_empty free2) then (
        let var = StringSet.choose free2 in
        raise (IllFormed (OpenParallel var))
      );
      check_global_free bound g1;
      check_global_free bound g2

(** Check for unguarded recursion in global types *)
let rec is_guarded_global (var : string) (g : string global) : bool =
  match g with
  | GEnd _ -> true
  | GVar (v, _) -> v <> var  (* If we reach the variable, it's unguarded *)
  | GRec (v, body, _) ->
      if v = var then true  (* Shadowed, so safe *)
      else is_guarded_global var body
  | GMsg (_, _, _, _cont, _) ->
      true  (* Message guards the continuation *)
  | GBra (_, _, _branches, _) ->
      true  (* Branching itself guards *)
  | GPar (g1, g2, _) ->
      is_guarded_global var g1 && is_guarded_global var g2

(** Check global type is well-formed *)
let check_global (g : string global) : unit =
  (* Check for free variables *)
  check_global_free StringSet.empty g;
  
  (* Check for unguarded recursion *)
  let rec check_unguarded = function
    | GEnd _ -> ()
    | GVar _ -> ()
    | GRec (v, body, _) ->
        if not (is_guarded_global v body) then
          raise (IllFormed (UnguardedRecursion v));
        check_unguarded body
    | GMsg (_, _, _, cont, _) ->
        check_unguarded cont
    | GBra (_, _, branches, _) ->
        List.iter (fun (_, g) -> check_unguarded g) branches
    | GPar (g1, g2, _) ->
        check_unguarded g1;
        check_unguarded g2
  in
  check_unguarded g

(** Check for free variables in local types *)
let rec check_local_free (bound : StringSet.t) (l : string local) : unit =
  match l with
  | LEnd _ -> ()
  | LVar (v, _) ->
      if not (StringSet.mem v bound) then
        raise (IllFormed (FreeVariable (v, "local type")))
  | LRec (v, body, _) ->
      check_local_free (StringSet.add v bound) body
  | LRecv (_, _, cont, _) | LSend (_, _, cont, _) ->
      check_local_free bound cont
  | LExt (_, branches, _) ->
      if branches = [] then
        raise (IllFormed (EmptyChoice "local external choice"));
      check_unique_labels branches "local external choice";
      List.iter (fun (_, l) -> check_local_free bound l) branches
  | LInt (_, branches, _) ->
      if branches = [] then
        raise (IllFormed (EmptyChoice "local internal choice"));
      check_unique_labels branches "local internal choice";
      List.iter (fun (_, l) -> check_local_free bound l) branches

(** Check for unguarded recursion in local types *)
let rec is_guarded_local (var : string) (l : string local) : bool =
  match l with
  | LEnd _ -> true
  | LVar (v, _) -> v <> var
  | LRec (v, body, _) ->
      if v = var then true
      else is_guarded_local var body
  | LRecv (_, _, _, _) | LSend (_, _, _, _) ->
      true  (* Communication guards *)
  | LExt (_, _branches, _) | LInt (_, _branches, _) ->
      true  (* Choice guards *)

(** Check local type is well-formed *)
let check_local (l : string local) : unit =
  (* Check for free variables *)
  check_local_free StringSet.empty l;
  
  (* Check for unguarded recursion *)
  let rec check_unguarded = function
    | LEnd _ -> ()
    | LVar _ -> ()
    | LRec (v, body, _) ->
        if not (is_guarded_local v body) then
          raise (IllFormed (UnguardedRecursion v));
        check_unguarded body
    | LRecv (_, _, cont, _) | LSend (_, _, cont, _) ->
        check_unguarded cont
    | LExt (_, branches, _) | LInt (_, branches, _) ->
        List.iter (fun (_, l) -> check_unguarded l) branches
  in
  check_unguarded l

(** Check for free variables in processes *)
let rec check_process_free (bound : StringSet.t) (p : string processes) : unit =
  match p with
  | PInact _ -> ()
  | PVar (v, _) ->
      if not (StringSet.mem v bound) then
        raise (IllFormed (FreeVariable (v, "process")))
  | PRec (v, body, _) ->
      check_process_free (StringSet.add v bound) body
  | PSend (_, _, cont, _) ->
      (* Note: we don't check expression variables here *)
      check_process_free bound cont
  | PRecv (_, _, cont, _) ->
      (* Note: we don't check the received variable binding *)
      check_process_free bound cont
  | PInt (_, _, cont, _) ->
      (* Internal choice selects exactly one label *)
      check_process_free bound cont
  | PExt (_, branches, _) ->
      if branches = [] then
        raise (IllFormed (EmptyChoice "process external choice"));
      check_unique_labels branches "process external choice";
      List.iter (fun (_, p) -> check_process_free bound p) branches
  | PIfThenElse (_, then_proc, else_proc, _) ->
      check_process_free bound then_proc;
      check_process_free bound else_proc

(** Check for unguarded recursion in processes *)
let rec is_guarded_process (var : string) (p : string processes) : bool =
  match p with
  | PInact _ -> true
  | PVar (v, _) -> v <> var
  | PRec (v, body, _) ->
      if v = var then true
      else is_guarded_process var body
  | PSend (_, _, _, _) | PRecv (_, _, _, _) ->
      true  (* Communication guards *)
  | PInt (_, _, _, _) | PExt (_, _, _) ->
      true  (* Choice guards *)
  | PIfThenElse (_, then_proc, else_proc, _) ->
      (* If-then-else doesn't guard - both branches must be guarded *)
      is_guarded_process var then_proc && is_guarded_process var else_proc

(** Check process is well-formed *)
let check_process (p : string processes) : unit =
  (* Check for free variables *)
  check_process_free StringSet.empty p;
  
  (* Check for unguarded recursion *)
  let rec check_unguarded = function
    | PInact _ -> ()
    | PVar _ -> ()
    | PRec (v, body, _) ->
        if not (is_guarded_process v body) then
          raise (IllFormed (UnguardedRecursion v));
        check_unguarded body
    | PSend (_, _, cont, _) | PRecv (_, _, cont, _) ->
        check_unguarded cont
    | PInt (_, _, cont, _) ->
        check_unguarded cont
    | PExt (_, branches, _) ->
        List.iter (fun (_, p) -> check_unguarded p) branches
    | PIfThenElse (_, then_proc, else_proc, _) ->
        check_unguarded then_proc;
        check_unguarded else_proc
  in
  check_unguarded p

(** Check for duplicate process names *)
let check_unique_process_names (defs : string process_definition list) : unit =
  let rec check_dups seen = function
    | [] -> ()
    | def :: rest ->
        if StringSet.mem def.name seen then
          raise (IllFormed (DuplicateLabel (def.name, "process definitions")))
        else
          check_dups (StringSet.add def.name seen) rest
  in
  check_dups StringSet.empty defs

(** Check that all referenced processes are defined *)
let check_process_refs (defs : string process_definition list) (main : string tagged_process list) : unit =
  let defined_names = 
    List.fold_left (fun acc def -> StringSet.add def.name acc) StringSet.empty defs
  in
  List.iter (fun tagged ->
    if not (StringSet.mem tagged.process_ref defined_names) then
      raise (IllFormed (FreeVariable (tagged.process_ref, "main composition")))
  ) main

(** Check if a program is well-formed *)
let check_program (prog : string program) : unit =
  (* Check for duplicate process names *)
  check_unique_process_names prog.definitions;
  
  (* Check all process definitions are well-formed *)
  List.iter (fun def -> 
    (* Check the process body *)
    check_process def.body;
    (* Check the type annotation if present *)
    match def.type_annotation with
    | None -> ()
    | Some ty -> check_local ty
  ) prog.definitions;
  
  (* Check that all processes referenced in main are defined *)
  check_process_refs prog.definitions prog.main

