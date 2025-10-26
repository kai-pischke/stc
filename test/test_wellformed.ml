(* test_wellformed.ml: Tests for well-formedness checking *)

open Stc

let dummy = Loc.dummy

(** Test that well-formed types pass *)
let test_wellformed_passes _name check input () =
  try
    let ast = input in
    check ast;
    ()  (* Success *)
  with
  | Wellformed.IllFormed err ->
      Alcotest.fail (Printf.sprintf "Expected well-formed but got: %s" 
        (Wellformed.string_of_error err))

(** Test that ill-formed types fail with expected error *)
let test_wellformed_fails _name check input _expected_error () =
  try
    let ast = input in
    check ast;
    Alcotest.fail "Expected IllFormed exception but none was raised"
  with
  | Wellformed.IllFormed _err ->
      (* Just check it fails; could check specific error type if needed *)
      ()

(** Well-formed global type tests *)
let wellformed_global_tests = [
  (* Valid cases *)
  "end is well-formed", `Quick,
    test_wellformed_passes "end" Wellformed.check_global
      (Ast.GEnd dummy);
  
  "simple recursion", `Quick,
    test_wellformed_passes "rec" Wellformed.check_global
      (Ast.GRec ("X", Ast.GEnd dummy, dummy));
  
  "guarded recursion", `Quick,
    test_wellformed_passes "guarded" Wellformed.check_global
      (Ast.GRec ("X", 
        Ast.GMsg ("p", "q", "int", Ast.GVar ("X", dummy), dummy),
        dummy));
  
  "different roles communicate", `Quick,
    test_wellformed_passes "roles" Wellformed.check_global
      (Ast.GMsg ("p", "q", "int", Ast.GEnd dummy, dummy));
  
  "branching with choices", `Quick,
    test_wellformed_passes "branch" Wellformed.check_global
      (Ast.GBra ("p", "q", [("l1", Ast.GEnd dummy); ("l2", Ast.GEnd dummy)], dummy));
  
  (* Invalid cases *)
  "free variable fails", `Quick,
    test_wellformed_fails "free var" Wellformed.check_global
      (Ast.GVar ("X", dummy))
      (Wellformed.FreeVariable ("X", "global type"));
  
  "unguarded recursion fails", `Quick,
    test_wellformed_fails "unguarded" Wellformed.check_global
      (Ast.GRec ("X", Ast.GVar ("X", dummy), dummy))
      (Wellformed.UnguardedRecursion "X");
  
  "self-communication fails", `Quick,
    test_wellformed_fails "self-send" Wellformed.check_global
      (Ast.GMsg ("p", "p", "int", Ast.GEnd dummy, dummy))
      (Wellformed.SelfCommunication "p");
  
  "self-branching fails", `Quick,
    test_wellformed_fails "self-branch" Wellformed.check_global
      (Ast.GBra ("p", "p", [("l", Ast.GEnd dummy)], dummy))
      (Wellformed.SelfCommunication "p");
  
  "empty choice fails", `Quick,
    test_wellformed_fails "empty" Wellformed.check_global
      (Ast.GBra ("p", "q", [], dummy))
      (Wellformed.EmptyChoice "global branching");
  
  "duplicate labels fail", `Quick,
    test_wellformed_fails "dup labels" Wellformed.check_global
      (Ast.GBra ("p", "q", 
        [("ok", Ast.GEnd dummy); ("ok", Ast.GEnd dummy)],  (* duplicate "ok" *)
        dummy))
      (Wellformed.DuplicateLabel ("ok", "global branching"));
]

(** Well-formed local type tests *)
let wellformed_local_tests = [
  (* Valid cases *)
  "end is well-formed", `Quick,
    test_wellformed_passes "end" Wellformed.check_local
      (Ast.LEnd dummy);
  
  "simple recursion", `Quick,
    test_wellformed_passes "rec" Wellformed.check_local
      (Ast.LRec ("T", Ast.LEnd dummy, dummy));
  
  "guarded recursion", `Quick,
    test_wellformed_passes "guarded" Wellformed.check_local
      (Ast.LRec ("T",
        Ast.LRecv ("p", "int", Ast.LVar ("T", dummy), dummy),
        dummy));
  
  "receive message", `Quick,
    test_wellformed_passes "recv" Wellformed.check_local
      (Ast.LRecv ("p", "int", Ast.LEnd dummy, dummy));
  
  "send message", `Quick,
    test_wellformed_passes "send" Wellformed.check_local
      (Ast.LSend ("p", "bool", Ast.LEnd dummy, dummy));
  
  "choice with branches", `Quick,
    test_wellformed_passes "choice" Wellformed.check_local
      (Ast.LExt ("p", [("l1", Ast.LEnd dummy); ("l2", Ast.LEnd dummy)], dummy));
  
  (* Invalid cases *)
  "free variable fails", `Quick,
    test_wellformed_fails "free" Wellformed.check_local
      (Ast.LVar ("T", dummy))
      (Wellformed.FreeVariable ("T", "local type"));
  
  "unguarded recursion fails", `Quick,
    test_wellformed_fails "unguarded" Wellformed.check_local
      (Ast.LRec ("T", Ast.LVar ("T", dummy), dummy))
      (Wellformed.UnguardedRecursion "T");
  
  "empty external choice fails", `Quick,
    test_wellformed_fails "empty ext" Wellformed.check_local
      (Ast.LExt ("p", [], dummy))
      (Wellformed.EmptyChoice "local external choice");
  
  "empty internal choice fails", `Quick,
    test_wellformed_fails "empty int" Wellformed.check_local
      (Ast.LInt ("p", [], dummy))
      (Wellformed.EmptyChoice "local internal choice");
  
  "duplicate labels in external choice fail", `Quick,
    test_wellformed_fails "dup ext" Wellformed.check_local
      (Ast.LExt ("p",
        [("opt1", Ast.LEnd dummy); ("opt1", Ast.LEnd dummy)],
        dummy))
      (Wellformed.DuplicateLabel ("opt1", "local external choice"));
  
  "duplicate labels in internal choice fail", `Quick,
    test_wellformed_fails "dup int" Wellformed.check_local
      (Ast.LInt ("p",
        [("label", Ast.LEnd dummy); ("label", Ast.LEnd dummy)],
        dummy))
      (Wellformed.DuplicateLabel ("label", "local internal choice"));
]

(** Well-formed process tests *)
let wellformed_process_tests = [
  (* Valid cases *)
  "inactive is well-formed", `Quick,
    test_wellformed_passes "inactive" Wellformed.check_process
      (Ast.PInact dummy);
  
  "simple recursion", `Quick,
    test_wellformed_passes "rec" Wellformed.check_process
      (Ast.PRec ("X", Ast.PInact dummy, dummy));
  
  "guarded recursion", `Quick,
    test_wellformed_passes "guarded" Wellformed.check_process
      (Ast.PRec ("X",
        Ast.PSend ("p", Ast.EInt (42, dummy), Ast.PVar ("X", dummy), dummy),
        dummy));
  
  "send process", `Quick,
    test_wellformed_passes "send" Wellformed.check_process
      (Ast.PSend ("p", Ast.EInt (42, dummy), Ast.PInact dummy, dummy));
  
  "receive process", `Quick,
    test_wellformed_passes "recv" Wellformed.check_process
      (Ast.PRecv ("p", "x", Ast.PInact dummy, dummy));
  
  "choice with branches", `Quick,
    test_wellformed_passes "choice" Wellformed.check_process
      (Ast.PInt ("p", [("ok", Ast.PInact dummy); ("error", Ast.PInact dummy)], dummy));
  
  (* Invalid cases *)
  "free variable fails", `Quick,
    test_wellformed_fails "free" Wellformed.check_process
      (Ast.PVar ("X", dummy))
      (Wellformed.FreeVariable ("X", "process"));
  
  "unguarded recursion fails", `Quick,
    test_wellformed_fails "unguarded" Wellformed.check_process
      (Ast.PRec ("X", Ast.PVar ("X", dummy), dummy))
      (Wellformed.UnguardedRecursion "X");
  
  "empty internal choice fails", `Quick,
    test_wellformed_fails "empty int" Wellformed.check_process
      (Ast.PInt ("p", [], dummy))
      (Wellformed.EmptyChoice "process internal choice");
  
  "empty external choice fails", `Quick,
    test_wellformed_fails "empty ext" Wellformed.check_process
      (Ast.PExt ("p", [], dummy))
      (Wellformed.EmptyChoice "process external choice");
  
  "duplicate labels in internal choice fail", `Quick,
    test_wellformed_fails "dup int" Wellformed.check_process
      (Ast.PInt ("p",
        [("action", Ast.PInact dummy); ("action", Ast.PInact dummy)],
        dummy))
      (Wellformed.DuplicateLabel ("action", "process internal choice"));
  
  "duplicate labels in external choice fail", `Quick,
    test_wellformed_fails "dup ext" Wellformed.check_process
      (Ast.PExt ("p",
        [("choice", Ast.PInact dummy); ("choice", Ast.PInact dummy)],
        dummy))
      (Wellformed.DuplicateLabel ("choice", "process external choice"));
]

(** Complex well-formedness tests *)
let complex_tests = [
  "nested recursion", `Quick,
    test_wellformed_passes "nested" Wellformed.check_global
      (Ast.GRec ("X",
        Ast.GRec ("Y",
          Ast.GMsg ("p", "q", "int", Ast.GVar ("X", dummy), dummy),
          dummy),
        dummy));
  
  "shadowed variable with guard", `Quick,
    test_wellformed_passes "shadow" Wellformed.check_global
      (Ast.GRec ("X",
        Ast.GMsg ("p", "q", "int",
          Ast.GRec ("X",  (* Shadows outer X *)
            Ast.GMsg ("r", "s", "bool", Ast.GVar ("X", dummy), dummy),  (* Guarded reference to inner X *)
            dummy),
          dummy),
        dummy));
  
  "parallel with different types", `Quick,
    test_wellformed_passes "parallel" Wellformed.check_global
      (Ast.GPar (
        Ast.GMsg ("p", "q", "int", Ast.GEnd dummy, dummy),
        Ast.GMsg ("r", "s", "bool", Ast.GEnd dummy, dummy),
        dummy));
  
  "open parallel left side fails", `Quick,
    test_wellformed_fails "open left" Wellformed.check_global
      (Ast.GRec ("X",
        Ast.GPar (
          Ast.GVar ("X", dummy),
          Ast.GEnd dummy,
          dummy),
        dummy))
      (Wellformed.OpenParallel "X");
  
  "open parallel right side fails", `Quick,
    test_wellformed_fails "open right" Wellformed.check_global
      (Ast.GRec ("X",
        Ast.GPar (
          Ast.GEnd dummy,
          Ast.GVar ("X", dummy),
          dummy),
        dummy))
      (Wellformed.OpenParallel "X");
  
  "open parallel both sides fails", `Quick,
    test_wellformed_fails "open both" Wellformed.check_global
      (Ast.GRec ("X",
        Ast.GPar (
          Ast.GVar ("X", dummy),
          Ast.GVar ("X", dummy),
          dummy),
        dummy))
      (Wellformed.OpenParallel "X");
  
  "open parallel nested vars fails", `Quick,
    test_wellformed_fails "open nested" Wellformed.check_global
      (Ast.GRec ("X",
        Ast.GRec ("Y",
          Ast.GMsg ("p", "q", "int",
            Ast.GPar (Ast.GVar ("X", dummy), Ast.GVar ("Y", dummy), dummy),
            dummy),
          dummy),
        dummy))
      (Wellformed.OpenParallel "X");
  
  "closed parallel in recursion passes", `Quick,
    test_wellformed_passes "closed in rec" Wellformed.check_global
      (Ast.GRec ("X",
        Ast.GPar (
          Ast.GMsg ("p", "q", "int", Ast.GEnd dummy, dummy),
          Ast.GMsg ("r", "s", "bool", Ast.GEnd dummy, dummy),
          dummy),
        dummy));
]

let () =
  Alcotest.run "Well-formedness Checker" [
    "global types", wellformed_global_tests;
    "local types", wellformed_local_tests;
    "processes", wellformed_process_tests;
    "complex cases", complex_tests;
  ]

