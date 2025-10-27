(* pretty.ml: Pretty printing for session types and processes *)

open Ast

(* Pretty print a list of branches with a separator *)
let rec pp_branches pp_elem fmt = function
  | [] -> ()
  | [(label, elem)] -> 
      Format.fprintf fmt "%s: %a" label pp_elem elem
  | (label, elem) :: rest ->
      Format.fprintf fmt "%s: %a, %a" label pp_elem elem (pp_branches pp_elem) rest

(* Pretty print global types *)
let rec pp_global fmt = function
  | GEnd _ -> 
      Format.fprintf fmt "end"
  | GVar (v, _) -> 
      Format.fprintf fmt "%s" v
  | GRec (v, body, _) -> 
      Format.fprintf fmt "rec %s.%a" v pp_global body
  | GMsg (sender, receiver, base_type, cont, _) -> 
      Format.fprintf fmt "%s -> %s:[%s]; %a" 
        sender receiver base_type pp_global cont
  | GBra (sender, receiver, branches, _) ->
      Format.fprintf fmt "%s -> %s{%a}" 
        sender receiver (pp_branches pp_global) branches
  | GPar (g1, g2, _) -> 
      Format.fprintf fmt "(%a | %a)" pp_global g1 pp_global g2

(* Pretty print local types *)
let rec pp_local fmt = function
  | LEnd _ -> 
      Format.fprintf fmt "end"
  | LVar (v, _) -> 
      Format.fprintf fmt "%s" v
  | LRec (v, body, _) -> 
      Format.fprintf fmt "rec %s.%a" v pp_local body
  | LRecv (role, base_type, cont, _) -> 
      Format.fprintf fmt "%s?[%s]; %a" role base_type pp_local cont
  | LSend (role, base_type, cont, _) -> 
      Format.fprintf fmt "%s![%s]; %a" role base_type pp_local cont
  | LExt (role, branches, _) ->
      Format.fprintf fmt "%s?{%a}" role (pp_branches pp_local) branches
  | LInt (role, branches, _) ->
      Format.fprintf fmt "%s!{%a}" role (pp_branches pp_local) branches

(* Pretty print expressions *)
let rec pp_expr fmt = function
  | ETrue _ -> 
      Format.fprintf fmt "true"
  | EFalse _ -> 
      Format.fprintf fmt "false"
  | EInt (n, _) -> 
      Format.fprintf fmt "%d" n
  | EVar (x, _) -> 
      Format.fprintf fmt "%s" x
  | ENot (e, _) -> 
      Format.fprintf fmt "not %a" pp_expr e
  | EOr (e1, e2, _) -> 
      Format.fprintf fmt "(%a or %a)" pp_expr e1 pp_expr e2
  | EAnd (e1, e2, _) -> 
      Format.fprintf fmt "(%a and %a)" pp_expr e1 pp_expr e2
  | EPlus (e1, e2, _) -> 
      Format.fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2
  | EMinus (e1, e2, _) -> 
      Format.fprintf fmt "(%a - %a)" pp_expr e1 pp_expr e2
  | ETimes (e1, e2, _) -> 
      Format.fprintf fmt "(%a * %a)" pp_expr e1 pp_expr e2
  | EDiv (e1, e2, _) -> 
      Format.fprintf fmt "(%a / %a)" pp_expr e1 pp_expr e2
  | EMod (e1, e2, _) -> 
      Format.fprintf fmt "(%a mod %a)" pp_expr e1 pp_expr e2
  | ELt (e1, e2, _) -> 
      Format.fprintf fmt "(%a < %a)" pp_expr e1 pp_expr e2
  | EGt (e1, e2, _) -> 
      Format.fprintf fmt "(%a > %a)" pp_expr e1 pp_expr e2
  | ELe (e1, e2, _) -> 
      Format.fprintf fmt "(%a <= %a)" pp_expr e1 pp_expr e2
  | EGe (e1, e2, _) -> 
      Format.fprintf fmt "(%a >= %a)" pp_expr e1 pp_expr e2
  | EEq (e1, e2, _) -> 
      Format.fprintf fmt "(%a = %a)" pp_expr e1 pp_expr e2
  | EChoice (e1, e2, _) ->
      Format.fprintf fmt "(%a nondet %a)" pp_expr e1 pp_expr e2

(* Pretty print processes *)
let rec pp_process fmt = function
  | PInact _ -> 
      Format.fprintf fmt "0"
  | PVar (v, _) -> 
      Format.fprintf fmt "%s" v
  | PRec (v, body, _) -> 
      Format.fprintf fmt "rec %s.%a" v pp_process body
  | PSend (role, expr, cont, _) -> 
      Format.fprintf fmt "%s![%a].%a" role pp_expr expr pp_process cont
  | PRecv (role, var, cont, _) -> 
      Format.fprintf fmt "%s?(%s).%a" role var pp_process cont
  | PInt (role, label, cont, _) ->
      Format.fprintf fmt "%s:%s.%a" role label pp_process cont
  | PExt (role, branches, _) ->
      Format.fprintf fmt "%s?{%a}" role (pp_branches pp_process) branches
  | PIfThenElse (cond, then_proc, else_proc, _) ->
      Format.fprintf fmt "if %a then %a else %a" 
        pp_expr cond pp_process then_proc pp_process else_proc

(* Pretty print a process definition *)
let pp_process_definition fmt def =
  match def.Ast.type_annotation with
  | None ->
      Format.fprintf fmt "@[%s = %a@]" def.Ast.name pp_process def.Ast.body
  | Some ty ->
      Format.fprintf fmt "@[%s :: %a@]@\n@[%s = %a@]" 
        def.Ast.name pp_local ty
        def.Ast.name pp_process def.Ast.body

(* Pretty print a tagged process *)
let pp_tagged_process fmt tagged =
  Format.fprintf fmt "@[%s :: %s@]" tagged.Ast.participant tagged.Ast.process_ref

(* Pretty print a program *)
let pp_program fmt prog =
  (* Print all process definitions *)
  List.iter (fun def ->
    Format.fprintf fmt "%a@\n@\n" pp_process_definition def
  ) prog.Ast.definitions;
  
  (* Print main *)
  Format.fprintf fmt "@[<hov 2>main = ";
  
  (* Print tagged processes separated by |  *)
  (match prog.Ast.main with
  | [] -> Format.fprintf fmt "(* empty main *)"
  | [t] -> pp_tagged_process fmt t
  | t :: ts ->
      pp_tagged_process fmt t;
      List.iter (fun t ->
        Format.fprintf fmt " | %a" pp_tagged_process t
      ) ts);
  
  Format.fprintf fmt "@]"

(* Convenience functions for printing to string *)
let string_of_global g = 
  Format.asprintf "%a" pp_global g

let string_of_local l = 
  Format.asprintf "%a" pp_local l

let string_of_process p = 
  Format.asprintf "%a" pp_process p

let string_of_expr e = 
  Format.asprintf "%a" pp_expr e

let string_of_program prog =
  Format.asprintf "%a" pp_program prog

