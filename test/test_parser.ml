(* test.ml: Test suite using Alcotest *)

open Stc

(** Helper to create test AST nodes *)
let dummy = Loc.dummy

(** Alcotest testable for AST types *)
let global_testable = Alcotest.testable 
  (fun fmt g -> Format.fprintf fmt "%a" Pretty.pp_global g)
  (=)

let local_testable = Alcotest.testable
  (fun fmt l -> Format.fprintf fmt "%a" Pretty.pp_local l)
  (=)

let process_testable = Alcotest.testable
  (fun fmt p -> Format.fprintf fmt "%a" Pretty.pp_process p)
  (=)

(** Test helper to check AST equality *)
let test_parse_global input expected () =
  try
    let result = Parse.global_from_string input in
    Alcotest.(check global_testable) "parsed AST" expected result
  with
  | Parse.ParseError err ->
      Alcotest.fail (Parse.string_of_error err)

let test_parse_local input expected () =
  try
    let result = Parse.local_from_string input in
    Alcotest.(check local_testable) "parsed AST" expected result
  with
  | Parse.ParseError err ->
      Alcotest.fail (Parse.string_of_error err)

let test_parse_process input expected () =
  try
    let result = Parse.process_from_string input in
    Alcotest.(check process_testable) "parsed AST" expected result
  with
  | Parse.ParseError err ->
      Alcotest.fail (Parse.string_of_error err)

(** Global type tests *)
let global_tests = [
  "simple end", `Quick, 
    test_parse_global "end" (Ast.GEnd dummy);
  
  "type variable", `Quick,
    test_parse_global "X" (Ast.GVar ("X", dummy));
  
  "recursive type", `Quick,
    test_parse_global "rec X.end" 
      (Ast.GRec ("X", Ast.GEnd dummy, dummy));
  
  "message passing", `Quick,
    test_parse_global "p -> q:[int]; end"
      (Ast.GMsg ("p", "q", "int", Ast.GEnd dummy, dummy));
  
  "branching", `Quick,
    test_parse_global "p -> q{l1:end, l2:end}"
      (Ast.GBra ("p", "q", 
        [("l1", Ast.GEnd dummy); ("l2", Ast.GEnd dummy)], 
        dummy));
  
  "parallel composition", `Quick,
    test_parse_global "end | end"
      (Ast.GPar (Ast.GEnd dummy, Ast.GEnd dummy, dummy));
  
  "complex nested", `Quick,
    test_parse_global "rec X.p -> q:[bool]; (end | X)"
      (Ast.GRec ("X",
        Ast.GMsg ("p", "q", "bool",
          Ast.GPar (Ast.GEnd dummy, Ast.GVar ("X", dummy), dummy),
          dummy),
        dummy));
]

(** Local type tests *)
let local_tests = [
  "simple end", `Quick,
    test_parse_local "end" (Ast.LEnd dummy);
  
  "type variable", `Quick,
    test_parse_local "X" (Ast.LVar ("X", dummy));
  
  "receive message", `Quick,
    test_parse_local "p?[int]; end"
      (Ast.LRecv ("p", "int", Ast.LEnd dummy, dummy));
  
  "send message", `Quick,
    test_parse_local "p![bool]; end"
      (Ast.LSend ("p", "bool", Ast.LEnd dummy, dummy));
  
  "external choice", `Quick,
    test_parse_local "p?{l1:end, l2:end}"
      (Ast.LExt ("p", 
        [("l1", Ast.LEnd dummy); ("l2", Ast.LEnd dummy)],
        dummy));
  
  "internal choice", `Quick,
    test_parse_local "p!{ok:end, error:end}"
      (Ast.LInt ("p",
        [("ok", Ast.LEnd dummy); ("error", Ast.LEnd dummy)],
        dummy));
  
  "recursive local type", `Quick,
    test_parse_local "rec T.p?[int]; T"
      (Ast.LRec ("T",
        Ast.LRecv ("p", "int", Ast.LVar ("T", dummy), dummy),
        dummy));
]

(** Process tests *)
let process_tests = [
  "inactive process", `Quick,
    test_parse_process "0" (Ast.PInact dummy);
  
  "process variable", `Quick,
    test_parse_process "X" (Ast.PVar ("X", dummy));
  
  "recursive process", `Quick,
    test_parse_process "rec X.0"
      (Ast.PRec ("X", Ast.PInact dummy, dummy));
  
  "send integer", `Quick,
    test_parse_process "p![42].0"
      (Ast.PSend ("p", Ast.EInt (42, dummy), Ast.PInact dummy, dummy));
  
  "send boolean true", `Quick,
    test_parse_process "p![true].0"
      (Ast.PSend ("p", Ast.ETrue dummy, Ast.PInact dummy, dummy));
  
  "send boolean false", `Quick,
    test_parse_process "p![false].0"
      (Ast.PSend ("p", Ast.EFalse dummy, Ast.PInact dummy, dummy));
  
  "send variable", `Quick,
    test_parse_process "p![x].0"
      (Ast.PSend ("p", Ast.EVar ("x", dummy), Ast.PInact dummy, dummy));
  
  "receive", `Quick,
    test_parse_process "p?(x).0"
      (Ast.PRecv ("p", "x", Ast.PInact dummy, dummy));
  
  "internal choice", `Quick,
    test_parse_process "p:ok.0"
      (Ast.PInt ("p", "ok", Ast.PInact dummy, dummy));
  
  "external choice", `Quick,
    test_parse_process "p?{success:0, failure:0}"
      (Ast.PExt ("p",
        [("success", Ast.PInact dummy); ("failure", Ast.PInact dummy)],
        dummy));
]

(** Expression tests *)
let expression_tests = [
  "addition", `Quick,
    test_parse_process "p![x + y].0"
      (Ast.PSend ("p",
        Ast.EPlus (Ast.EVar ("x", dummy), Ast.EVar ("y", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "multiplication", `Quick,
    test_parse_process "p![x * y].0"
      (Ast.PSend ("p",
        Ast.ETimes (Ast.EVar ("x", dummy), Ast.EVar ("y", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "subtraction", `Quick,
    test_parse_process "p![x - y].0"
      (Ast.PSend ("p",
        Ast.EMinus (Ast.EVar ("x", dummy), Ast.EVar ("y", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "division", `Quick,
    test_parse_process "p![x / y].0"
      (Ast.PSend ("p",
        Ast.EDiv (Ast.EVar ("x", dummy), Ast.EVar ("y", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "modulo", `Quick,
    test_parse_process "p![x mod y].0"
      (Ast.PSend ("p",
        Ast.EMod (Ast.EVar ("x", dummy), Ast.EVar ("y", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "precedence: mult before add", `Quick,
    test_parse_process "p![x + y * z].0"
      (Ast.PSend ("p",
        Ast.EPlus (
          Ast.EVar ("x", dummy),
          Ast.ETimes (Ast.EVar ("y", dummy), Ast.EVar ("z", dummy), dummy),
          dummy),
        Ast.PInact dummy, dummy));
  
  "less than", `Quick,
    test_parse_process "p![a < b].0"
      (Ast.PSend ("p",
        Ast.ELt (Ast.EVar ("a", dummy), Ast.EVar ("b", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "greater than", `Quick,
    test_parse_process "p![a > b].0"
      (Ast.PSend ("p",
        Ast.EGt (Ast.EVar ("a", dummy), Ast.EVar ("b", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "less than or equal", `Quick,
    test_parse_process "p![a <= b].0"
      (Ast.PSend ("p",
        Ast.ELe (Ast.EVar ("a", dummy), Ast.EVar ("b", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "greater than or equal", `Quick,
    test_parse_process "p![a >= b].0"
      (Ast.PSend ("p",
        Ast.EGe (Ast.EVar ("a", dummy), Ast.EVar ("b", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "equality", `Quick,
    test_parse_process "p![x = y].0"
      (Ast.PSend ("p",
        Ast.EEq (Ast.EVar ("x", dummy), Ast.EVar ("y", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "logical and", `Quick,
    test_parse_process "p![a and b].0"
      (Ast.PSend ("p",
        Ast.EAnd (Ast.EVar ("a", dummy), Ast.EVar ("b", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "logical or", `Quick,
    test_parse_process "p![a or b].0"
      (Ast.PSend ("p",
        Ast.EOr (Ast.EVar ("a", dummy), Ast.EVar ("b", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "logical not", `Quick,
    test_parse_process "p![not x].0"
      (Ast.PSend ("p",
        Ast.ENot (Ast.EVar ("x", dummy), dummy),
        Ast.PInact dummy, dummy));
  
  "precedence: and before or", `Quick,
    test_parse_process "p![a or b and c].0"
      (Ast.PSend ("p",
        Ast.EOr (
          Ast.EVar ("a", dummy),
          Ast.EAnd (Ast.EVar ("b", dummy), Ast.EVar ("c", dummy), dummy),
          dummy),
        Ast.PInact dummy, dummy));
  
  "parentheses override precedence", `Quick,
    test_parse_process "p![(x + y) * z].0"
      (Ast.PSend ("p",
        Ast.ETimes (
          Ast.EPlus (Ast.EVar ("x", dummy), Ast.EVar ("y", dummy), dummy),
          Ast.EVar ("z", dummy),
          dummy),
        Ast.PInact dummy, dummy));
]

(** Main test suite *)
let () =
  Alcotest.run "Session Type Parser" [
    "global types", global_tests;
    "local types", local_tests;
    "processes", process_tests;
    "expressions", expression_tests;
  ]
