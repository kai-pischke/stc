(* interpreter.ml: Interpreter for programs *)

open Ast

(** Runtime values *)
type value =
  | VInt of int
  | VBool of bool

(** Runtime errors *)
type error =
  | TypeError of string
  | UndefinedVariable of string
  | UndefinedProcess of string
  | NoSuchParticipant of string
  | CommunicationMismatch of string
  | StuckProcess of string
  | InvalidChoice of string

exception RuntimeError of error

(** Format error for display *)
let string_of_error = function
  | TypeError msg -> Printf.sprintf "Type error: %s" msg
  | UndefinedVariable var -> Printf.sprintf "Undefined variable: %s" var
  | UndefinedProcess proc -> Printf.sprintf "Undefined process: %s" proc
  | NoSuchParticipant part -> Printf.sprintf "No such participant: %s" part
  | CommunicationMismatch msg -> Printf.sprintf "Communication mismatch: %s" msg
  | StuckProcess msg -> Printf.sprintf "Process stuck: %s" msg
  | InvalidChoice msg -> Printf.sprintf "Invalid choice: %s" msg

(** String representation of values *)
let string_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b

(** Environment for variables *)
module Env = Map.Make(String)

(** Evaluate an expression in an environment *)
let rec eval_expr (env : value Env.t) (expr : string expr) : value =
  match expr with
  | ETrue _ -> VBool true
  | EFalse _ -> VBool false
  | EInt (n, _) -> VInt n
  | EVar (x, _) ->
      (match Env.find_opt x env with
       | Some v -> v
       | None -> raise (RuntimeError (UndefinedVariable x)))
  
  | ENot (e, _) ->
      (match eval_expr env e with
       | VBool b -> VBool (not b)
       | VInt _ -> raise (RuntimeError (TypeError "not expects boolean")))
  
  | EOr (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VBool b1, VBool b2 -> VBool (b1 || b2)
       | VInt _, _ | _, VInt _ -> 
           raise (RuntimeError (TypeError "or expects booleans")))
  
  | EAnd (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VBool b1, VBool b2 -> VBool (b1 && b2)
       | VInt _, _ | _, VInt _ -> 
           raise (RuntimeError (TypeError "and expects booleans")))
  
  | EPlus (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VInt (n1 + n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "+ expects integers")))
  
  | EMinus (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VInt (n1 - n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "- expects integers")))
  
  | ETimes (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VInt (n1 * n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "* expects integers")))
  
  | EDiv (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> 
           if n2 = 0 then raise (RuntimeError (TypeError "division by zero"))
           else VInt (n1 / n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "/ expects integers")))
  
  | EMod (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> 
           if n2 = 0 then raise (RuntimeError (TypeError "modulo by zero"))
           else VInt (n1 mod n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "mod expects integers")))
  
  | ELt (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VBool (n1 < n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "< expects integers")))
  
  | EGt (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VBool (n1 > n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "> expects integers")))
  
  | ELe (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VBool (n1 <= n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError "<= expects integers")))
  
  | EGe (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VBool (n1 >= n2)
       | VBool _, _ | _, VBool _ -> 
           raise (RuntimeError (TypeError ">= expects integers")))
  
  | EEq (e1, e2, _) ->
      (match eval_expr env e1, eval_expr env e2 with
       | VInt n1, VInt n2 -> VBool (n1 = n2)
       | VBool b1, VBool b2 -> VBool (b1 = b2)
       | VInt _, VBool _ | VBool _, VInt _ ->
           raise (RuntimeError (TypeError "= expects same types")))
  
  | EChoice (e1, e2, _) ->
      (* Non-deterministic choice: randomly pick one branch *)
      if Random.bool () then eval_expr env e1
      else eval_expr env e2

(** Process definitions lookup *)
let find_process_def (defs : string process_definition list) (name : string) : string processes =
  match List.find_opt (fun def -> def.name = name) defs with
  | Some def -> def.body
  | None -> raise (RuntimeError (UndefinedProcess name))

(** Substitute a specific variable with a process (for unfolding recursion) *)
let rec subst_var (var : string) (replacement : string processes) (p : string processes) : string processes =
  match p with
  | PInact _ -> p
  | PVar (name, _) when name = var -> replacement
  | PVar _ -> p
  | PRec (v, body, loc) ->
      if v = var then
        (* Variable is shadowed, don't substitute inside *)
        p
      else
        PRec (v, subst_var var replacement body, loc)
  | PInt (role, label, cont, loc) ->
      PInt (role, label, subst_var var replacement cont, loc)
  | PExt (role, branches, loc) ->
      PExt (role, List.map (fun (l, p) -> (l, subst_var var replacement p)) branches, loc)
  | PSend (role, expr, cont, loc) ->
      PSend (role, expr, subst_var var replacement cont, loc)
  | PRecv (role, v, cont, loc) ->
      PRecv (role, v, subst_var var replacement cont, loc)
  | PIfThenElse (cond, then_proc, else_proc, loc) ->
      PIfThenElse (cond, subst_var var replacement then_proc, subst_var var replacement else_proc, loc)

(** Substitute process variables with their definitions, respecting bound variables *)
let rec subst_process_aux (bound : string list) (defs : string process_definition list) (p : string processes) : string processes =
  match p with
  | PInact _ -> p
  | PVar (name, _loc) -> 
      (* Only substitute if not bound by a rec *)
      if List.mem name bound then
        p
      else
        (* Look up the process definition *)
        find_process_def defs name
  | PRec (v, body, loc) -> 
      (* Add v to bound variables when processing body *)
      PRec (v, subst_process_aux (v :: bound) defs body, loc)
  | PInt (role, label, cont, loc) ->
      PInt (role, label, subst_process_aux bound defs cont, loc)
  | PExt (role, branches, loc) ->
      PExt (role, List.map (fun (l, p) -> (l, subst_process_aux bound defs p)) branches, loc)
  | PSend (role, expr, cont, loc) ->
      PSend (role, expr, subst_process_aux bound defs cont, loc)
  | PRecv (role, var, cont, loc) ->
      PRecv (role, var, subst_process_aux bound defs cont, loc)
  | PIfThenElse (cond, then_proc, else_proc, loc) ->
      PIfThenElse (cond, subst_process_aux bound defs then_proc, subst_process_aux bound defs else_proc, loc)

(** Substitute process variables with their definitions *)
let subst_process (defs : string process_definition list) (p : string processes) : string processes =
  subst_process_aux [] defs p

(** Execute a program *)
let execute_program (prog : string program) =
  Printf.printf "╔══════════════════════════════════════════╗\n";
  Printf.printf "║  Program Execution Starting              ║\n";
  Printf.printf "╚══════════════════════════════════════════╝\n\n";
  
  (* Initialize random seed for non-deterministic choice *)
  Random.self_init ();
  
  (* Build initial state: map participants to their processes *)
  let initial_state = 
    List.fold_left (fun acc tagged ->
      let proc = find_process_def prog.definitions tagged.process_ref in
      let proc_subst = subst_process prog.definitions proc in
      (tagged.participant, proc_subst, Env.empty) :: acc
    ) [] prog.main
  in
  
  (* Print initial configuration *)
  Printf.printf "Initial Configuration:\n";
  Printf.printf "─────────────────────\n";
  List.iter (fun (part, proc, _) ->
    Printf.printf "  %s: %s\n" part (Pretty.string_of_process proc)
  ) initial_state;
  Printf.printf "\n";
  
  (* Simple execution: run until all processes are inactive or stuck *)
  let rec execute_step state step_num =
    (* Check if all processes are inactive *)
    let all_inactive = List.for_all (fun (_, proc, _) ->
      match proc with PInact _ -> true | _ -> false
    ) state in
    
    if all_inactive then (
      Printf.printf "\n✓ All processes terminated successfully!\n";
      Printf.printf "Total steps: %d\n" step_num
    ) else (
      Printf.printf "Step %d:\n" (step_num + 1);
      Printf.printf "────────\n";
      
      (* Try to find a communication or local action *)
      (* For simplicity, we'll execute one random enabled action *)
      let new_state = try_step state in
      Printf.printf "\n";
      execute_step new_state (step_num + 1)
    )
  
  and try_step state =
    (* Find processes that can make progress *)
    (* Prioritize: if-then-else, sends, receives, choices *)
    
    (* Try if-then-else first (local action) *)
    let state_opt = try_if_then_else state in
    match state_opt with
    | Some s -> s
    | None ->
        (* Try internal choice (local action) *)
        let state_opt = try_internal_choice state in
        (match state_opt with
         | Some s -> s
         | None ->
             (* Try communication (send-receive pair) *)
             let state_opt = try_communication state in
             (match state_opt with
              | Some s -> s
              | None ->
                  Printf.printf "✗ System stuck - no process can make progress\n";
                  raise (RuntimeError (StuckProcess "no enabled actions"))))
  
  and try_if_then_else state =
    (* Find a participant with an if-then-else *)
    let rec find_if acc = function
      | [] -> None
      | (part, PIfThenElse (cond, then_proc, else_proc, _), env) :: rest ->
          (* Evaluate condition *)
          let cond_val = eval_expr env cond in
          (match cond_val with
           | VBool true ->
               Printf.printf "  %s: if-then-else → THEN branch\n" part;
               let then_subst = subst_process prog.definitions then_proc in
               let new_state = List.rev_append acc ((part, then_subst, env) :: rest) in
               Some new_state
           | VBool false ->
               Printf.printf "  %s: if-then-else → ELSE branch\n" part;
               let else_subst = subst_process prog.definitions else_proc in
               let new_state = List.rev_append acc ((part, else_subst, env) :: rest) in
               Some new_state
           | VInt _ ->
               raise (RuntimeError (TypeError "if condition must be boolean")))
      | (part, (PRec (v, body, _) as rec_proc), env) :: rest ->
          (* Unfold recursion: replace v with the whole recursion in body *)
          let unfolded = subst_var v rec_proc body in
          find_if acc ((part, unfolded, env) :: rest)
      | item :: rest -> find_if (item :: acc) rest
    in
    find_if [] state
  
  and try_internal_choice state =
    (* Find a participant with an internal choice and match with external choice *)
    let rec find_choice acc = function
      | [] -> None
      | (sender_part, PInt (receiver_role, label, sender_cont, _), sender_env) :: rest ->
          (* Look for matching external choice at receiver *)
          let recv_opt = find_external_choice sender_part receiver_role label (List.rev_append acc rest) in
          (match recv_opt with
           | Some (recv_part, recv_cont, recv_env, others) ->
               Printf.printf "  %s → %s: label '%s'\n" sender_part recv_part label;
               
               let sender_cont_subst = subst_process prog.definitions sender_cont in
               let recv_cont_subst = subst_process prog.definitions recv_cont in
               
               let new_state =
                 (sender_part, sender_cont_subst, sender_env) ::
                 (recv_part, recv_cont_subst, recv_env) ::
                 others
               in
               Some new_state
           | None ->
               (* No matching receiver yet, try next *)
               find_choice ((sender_part, PInt (receiver_role, label, sender_cont, Loc.dummy), sender_env) :: acc) rest)
      | (part, (PRec (v, body, _) as rec_proc), env) :: rest ->
          (* Unfold recursion: replace v with the whole recursion in body *)
          let unfolded = subst_var v rec_proc body in
          find_choice acc ((part, unfolded, env) :: rest)
      | item :: rest -> find_choice (item :: acc) rest
    
    and find_external_choice expected_sender expected_receiver expected_label candidates =
      (* Find a participant named expected_receiver with external choice offering expected_label *)
      let rec search acc = function
        | [] -> None
        | (recv_part, PExt (sender_role, branches, _), env) :: rest ->
            if recv_part = expected_receiver && sender_role = expected_sender then
              (* Check if the label is offered *)
              (match List.assoc_opt expected_label branches with
               | Some cont -> Some (recv_part, cont, env, List.rev_append acc rest)
               | None -> 
                   raise (RuntimeError (InvalidChoice 
                     (Printf.sprintf "Label '%s' not offered by %s" expected_label recv_part))))
            else
              search ((recv_part, PExt (sender_role, branches, Loc.dummy), env) :: acc) rest
        | (part, (PRec (v, body, _) as rec_proc), env) :: rest ->
            (* Unfold recursion: replace v with the whole recursion in body *)
            let unfolded = subst_var v rec_proc body in
            search acc ((part, unfolded, env) :: rest)
        | item :: rest -> search (item :: acc) rest
      in
      search [] candidates
    in
    find_choice [] state
  
  and try_communication state =
    (* Try to match a sender with a receiver *)
    (* sender_part sends to receiver_role, so we need to find:
       - a participant named receiver_role
       - that has a PRecv waiting for sender_part *)
    let rec find_comm acc = function
      | [] -> None
      | (sender_part, PSend (receiver_role, expr, sender_cont, _), sender_env) :: rest ->
          (* Eagerly evaluate the expression to catch type errors *)
          let value = eval_expr sender_env expr in
          
          (* Look for the receiver: a participant named receiver_role receiving from sender_part *)
          let recv_opt = find_receiver sender_part receiver_role (List.rev_append acc rest) in
          (match recv_opt with
           | Some (recv_part, recv_var, recv_cont, recv_env, others) ->
               (* Match found! *)
               Printf.printf "  %s → %s: %s\n" sender_part recv_part (string_of_value value);
               
               (* Update environments and processes *)
               let new_recv_env = Env.add recv_var value recv_env in
               let sender_cont_subst = subst_process prog.definitions sender_cont in
               let recv_cont_subst = subst_process prog.definitions recv_cont in
               
               let new_state = 
                 (sender_part, sender_cont_subst, sender_env) ::
                 (recv_part, recv_cont_subst, new_recv_env) ::
                 others
               in
               Some new_state
           | None ->
               (* No matching receiver yet, try next *)
               find_comm ((sender_part, PSend (receiver_role, expr, sender_cont, Loc.dummy), sender_env) :: acc) rest)
      
      | (part, (PRec (v, body, _) as rec_proc), env) :: rest ->
          (* Unfold recursion: replace v with the whole recursion in body *)
          let unfolded = subst_var v rec_proc body in
          find_comm acc ((part, unfolded, env) :: rest)
      
      | item :: rest -> find_comm (item :: acc) rest
    
    and find_receiver expected_sender expected_receiver candidates =
      (* Find a participant named expected_receiver that is receiving from expected_sender *)
      let rec search acc = function
        | [] -> None
        | (recv_part, PRecv (sender_role, var, cont, _), env) :: rest ->
            if recv_part = expected_receiver && sender_role = expected_sender then
              Some (recv_part, var, cont, env, List.rev_append acc rest)
            else
              search ((recv_part, PRecv (sender_role, var, cont, Loc.dummy), env) :: acc) rest
        | (part, (PRec (v, body, _) as rec_proc), env) :: rest ->
            (* Unfold recursion: replace v with the whole recursion in body *)
            let unfolded = subst_var v rec_proc body in
            search acc ((part, unfolded, env) :: rest)
        | item :: rest -> search (item :: acc) rest
      in
      search [] candidates
    in
    find_comm [] state
  
  in
  
  try
    execute_step initial_state 0
  with
  | RuntimeError err ->
      Printf.printf "\n✗ Runtime Error: %s\n" (string_of_error err);
      raise (RuntimeError err)

