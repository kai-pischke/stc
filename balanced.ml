(* balanced.ml: Check if a global type is balanced.
   
   A global type is balanced if at every branching point (GBra), 
   the reachable participants equal the unavoidable participants.
   
   - Reachable: roles that appear in at least one branch
   - Unavoidable: roles that appear in all branches
*)

open Ast

module RoleSet = Set.Make(String)

(* Exception for unbalanced types *)
exception Unbalanced of string

(* Check if a global type is balanced, returning detailed error message if not *)
let check_balanced (g: string global) : (bool * string option) =
  let rec check (g: string global) : (RoleSet.t * RoleSet.t) =
    match g with
    | GEnd _ -> (RoleSet.empty, RoleSet.empty)
    | GVar _ -> (RoleSet.empty, RoleSet.empty)
    | GRec (_, body, _) -> check body
    | GPar (g1, g2, _) -> 
        let (unavoidable1, reachable1) = check g1 in
        let (unavoidable2, reachable2) = check g2 in
        (RoleSet.union unavoidable1 unavoidable2, 
         RoleSet.union reachable1 reachable2)
    | GMsg (p, q, _, cont, _) ->
        let (unavoidable, reachable) = check cont in
        (RoleSet.add p (RoleSet.add q unavoidable), 
         RoleSet.add p (RoleSet.add q reachable))
    | GBra (p, q, branches, _) ->
        if branches = [] then
          raise (Unbalanced "Empty branch list in GBra")
        else
          let (unavoidable_branches, reachable_branches) = 
            List.fold_left (fun (unavoid_acc, reach_acc) (_, g') ->
              let (unavoid', reach') = check g' in
              match unavoid_acc with
              | None -> (Some unavoid', RoleSet.union reach_acc reach')
              | Some unavoid_set ->
                  (Some (RoleSet.inter unavoid_set unavoid'), 
                   RoleSet.union reach_acc reach')
            ) (None, RoleSet.empty) branches
          in
          let unavoidable = match unavoidable_branches with
            | None -> RoleSet.empty
            | Some s -> s
          in
          let reachable = reachable_branches in
          
          if not (RoleSet.equal unavoidable reachable) then begin
            let unavoid_str = String.concat ", " (RoleSet.elements unavoidable) in
            let reach_str = String.concat ", " (RoleSet.elements reachable) in
            raise (Unbalanced 
              (Printf.sprintf "At branch %s -> %s: unavoidable {%s} != reachable {%s}"
                p q unavoid_str reach_str))
          end;
          
          (RoleSet.add p (RoleSet.add q unavoidable), 
           RoleSet.add p (RoleSet.add q reachable))
  in
  try
    let _ = check g in
    (true, None)
  with
  | Unbalanced msg -> (false, Some msg)