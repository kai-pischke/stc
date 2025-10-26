(* Ast.ml: 
This file defines the abstract syntax for processes 
and for local and global types.*)

(* Locations are used for debugging. *)
open Loc

type role  = string (* p, q, ... *)
type label = string (* l1, l2, ...*)
type base  = string (* int, char, ...*)

(* Global types G1, G2, ...*)
type 'v global =
  | GEnd                                 of t
  | GVar       of 'v                     * t
  | GRec       of 'v * 'v global         * t
  | GBra       of role * role * (label * 'v global) list * t
  | GMsg       of role * role * base * 'v global * t
  | GPar       of 'v global * 'v global  * t

(* Local Types T1, T2, ...*)
type 'v local =
  (* end *)
  | LEnd                                 of t
  (* t *)
  | LVar       of 'v                     * t
  (* 𝜇t.T *)
  | LRec       of 'v * 'v local          * t
  (* p&{l1, ..., ln} *)
  | LInt       of role * (label * 'v local) list * t
  (* p⊕{l1, ..., ln} *)
  | LExt       of role * (label * 'v local) list * t
  (* p?⟨B⟩; T*)
  | LRecv      of role * base * 'v local * t
  (* p!⟨B⟩; T *)
  | LSend      of role * base * 'v local * t

(* Expressions e1, e2, ... *)
type 'v expr =
  (* true *)
  | ETrue      of t
  (* false *)
  | EFalse     of t
  (* e.g. 42 *)
  | EInt       of int * t 
  (* x *)
  | EVar       of string * t 
  (* ¬e *)
  | ENot       of 'v expr * t
  (* e1 ∨ e2 *)
  | EOr        of 'v expr * 'v expr * t
  (* e1 ∧ e2 *)
  | EAnd       of 'v expr * 'v expr * t
  (* e1 + e2 *)
  | EPlus      of 'v expr * 'v expr * t
  (* e1 - e2 *)
  | EMinus     of 'v expr * 'v expr * t
  (* e1 * e2 *)
  | ETimes     of 'v expr * 'v expr * t
  (* e1 / e2 *)
  | EDiv       of 'v expr * 'v expr * t
  (* e1 mod e2 *)
  | EMod       of 'v expr * 'v expr * t
  (* e1 < e2 *)
  | ELt        of 'v expr * 'v expr * t
  (* e1 > e2 *)
  | EGt        of 'v expr * 'v expr * t
  (* e1 <= e2 *)
  | ELe        of 'v expr * 'v expr * t
  (* e1 >= e2 *)
  | EGe        of 'v expr * 'v expr * t
  (* e1 = e2 *)
  | EEq        of 'v expr * 'v expr * t

(* Processes P1, P2, ...*)
type 'v processes =
  (* 0 *)
  | PInact     of t
  (* X *)
  | PVar       of 'v                     * t
  (* 𝜇X.P *)
  | PRec       of 'v * 'v processes      * t
  (* p ⊲ l.P *)
  | PInt       of role * (label * 'v processes) list * t
  (* p ⊳ {l1,...,ln} *)
  | PExt       of role * (label * 'v processes) list * t
  (* p!⟨e⟩.P *)
  | PSend      of role * 'v expr * 'v processes * t
  (* p(x).P *)
  | PRecv      of role * 'v * 'v processes * t
  (* if e then P1 else P2 *)
  | PIfThenElse of 'v expr * 'v processes * 'v processes * t

(* Process definition: P = body *)
type 'v process_definition = {
  name: string;
  body: 'v processes;
  loc: t;
}

(* Tagged process: participant :: process_name *)
type 'v tagged_process = {
  participant: string;
  process_ref: string;
  loc: t;
}

(* Complete program: definitions and main *)
type 'v program = {
  definitions: 'v process_definition list;
  main: 'v tagged_process list;  (* parallel composition *)
  loc: t;
}

(* --- boring utility functions ---*)
(* Extract the location from a global. *)
let loc_of_global = function
  | GEnd       l
  | GVar (_,l)
  | GRec (_,_,l)
  | GMsg (_,_,_,_,l)
  | GBra (_,_,_,l)
  | GPar (_,_,l) -> l

(* Extract the location from a local. *)
let loc_of_local = function
  | LEnd       l
  | LVar (_,l)
  | LRec (_,_,l)
  | LInt (_,_,l)
  | LExt (_,_,l)
  | LRecv (_,_,_,l)
  | LSend (_,_,_,l) -> l
