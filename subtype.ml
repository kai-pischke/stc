(** Subtyping for session types using the Gay and Hole 2005 algorithm
    
    This module implements coinductive subtyping for session types with recursion.
    The key insight is that when checking recursive types, we can assume the
    subtyping obligation holds (add it to the environment) while checking the
    unfolded bodies.
*)

open Ast

(** A subtyping obligation: T1 <= T2 *)
type obligation = int local * int local

(** Environment of assumed subtyping obligations
    
    Since De Bruijn types have normalized (sorted) branch lists,
    we can use simple structural equality for membership checking.
*)
module Env = struct
  type t = obligation list
  
  let empty : t = []
  
  let mem (t1 : int local) (t2 : int local) (env : t) : bool =
    List.mem (t1, t2) env
  
  let add (t1 : int local) (t2 : int local) (env : t) : t =
    (t1, t2) :: env
end

(** Main subtyping algorithm
    
    Check if t1 <= t2 (t1 is a subtype of t2) using the Gay and Hole algorithm.
    
    Rules:
    - If (t1, t2) is in the environment, return true (coinductive hypothesis)
    - If t1 = rec x.T1, add (t1, t2) to env and check unfold(t1) <= t2
    - If t2 = rec x.T2, add (t1, t2) to env and check t1 <= unfold(t2)
    - Otherwise, structural rules:
      - end <= end
      - p![B]; T1 <= p![B]; T2  if T1 <= T2
      - p?[B]; T1 <= p?[B]; T2  if T1 <= T2
      - p!{li:Ti} <= p!{lj:Tj}  if {li} ⊆ {lj} and ∀i. Ti <= Tj(i)  (covariant)
      - p?{li:Ti} <= p?{lj:Tj}  if {lj} ⊆ {li} and ∀j. Ti(j) <= Tj  (contravariant)
*)
let rec is_subtype_aux (env : Env.t) (t1 : int local) (t2 : int local) : bool =
  (* Check if this obligation is already assumed *)
  if Env.mem t1 t2 env then
    true
  else
    match t1, t2 with
    (* Handle recursion on the left: add assumption and check unfolded *)
    | LRec _, _ ->
        let env' = Env.add t1 t2 env in
        let t1_unfolded = Utils.unfold_debruijn_once t1 in
        is_subtype_aux env' t1_unfolded t2
    
    (* Handle recursion on the right: add assumption and check unfolded *)
    | _, LRec _ ->
        let env' = Env.add t1 t2 env in
        let t2_unfolded = Utils.unfold_debruijn_once t2 in
        is_subtype_aux env' t1 t2_unfolded
    
    (* Structural rules *)
    
    (* end <= end *)
    | LEnd _, LEnd _ -> true
    
    (* p![B]; T1 <= p![B]; T2  if T1 <= T2 *)
    | LSend (r1, b1, c1, _), LSend (r2, b2, c2, _) ->
        r1 = r2 && b1 = b2 && is_subtype_aux env c1 c2
    
    (* p?[B]; T1 <= p?[B]; T2  if T1 <= T2 *)
    | LRecv (r1, b1, c1, _), LRecv (r2, b2, c2, _) ->
        r1 = r2 && b1 = b2 && is_subtype_aux env c1 c2
    
    (* Internal choice (covariant):
       p!{l1:T1, ..., ln:Tn} <= p!{l1':T1', ..., lm':Tm'}
       if {l1,...,ln} ⊆ {l1',...,lm'} and ∀i. Ti <= Ti'
       
       Intuition: More options on the right means it can handle more cases
    *)
    | LInt (r1, branches1, _), LInt (r2, branches2, _) ->
        r1 = r2 &&
        (* Every label in t1 must be in t2 *)
        List.for_all (fun (label1, type1) ->
          match List.assoc_opt label1 branches2 with
          | Some type2 -> is_subtype_aux env type1 type2
          | None -> false  (* Label missing in t2 *)
        ) branches1
    
    (* External choice (contravariant):
       p?{l1:T1, ..., ln:Tn} <= p?{l1':T1', ..., lm':Tm'}
       if {l1',...,lm'} ⊆ {l1,...,ln} and ∀i. Ti(i) <= Ti'
       
       Intuition: Fewer options on the right means it offers fewer branches,
       which is safe (we can handle more than it needs)
    *)
    | LExt (r1, branches1, _), LExt (r2, branches2, _) ->
        r1 = r2 &&
        (* Every label in t2 must be in t1 *)
        List.for_all (fun (label2, type2) ->
          match List.assoc_opt label2 branches1 with
          | Some type1 -> is_subtype_aux env type1 type2
          | None -> false  (* Label missing in t1 *)
        ) branches2
    
    (* All other combinations are not subtypes *)
    | _, _ -> false

(** Public API: Check if t1 is a subtype of t2
    
    Both types should use named variables (string local).
    They are converted to De Bruijn representation internally.
    
    @param t1 The subtype
    @param t2 The supertype
    @return true if t1 <= t2, false otherwise
*)
let is_subtype (t1 : string local) (t2 : string local) : bool =
  let t1_db = Utils.to_debruijn t1 in
  let t2_db = Utils.to_debruijn t2 in
  is_subtype_aux Env.empty t1_db t2_db

(** Exception for subtyping errors *)
exception SubtypeError of string

(** Check subtyping and raise an exception if it fails
    
    @param t1 The subtype
    @param t2 The supertype
    @raise SubtypeError if t1 is not a subtype of t2
*)
let check_subtype (t1 : string local) (t2 : string local) : unit =
  if not (is_subtype t1 t2) then
    raise (SubtypeError (
      Printf.sprintf "%s is not a subtype of %s"
        (Pretty.string_of_local t1)
        (Pretty.string_of_local t2)
    ))

