(* test_interpreter.ml: Interpreter tests using Alcotest *)

open Stc

(** Helper: Check that a program executes successfully *)
let test_executes_ok prog_str () =
  let prog = Parse.program_from_string prog_str in
  (* Redirect stdout to suppress execution trace during tests *)
  let old_stdout = Unix.dup Unix.stdout in
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
  Unix.dup2 devnull Unix.stdout;
  Unix.close devnull;
  
  try
    Interpreter.execute_program prog;
    Unix.dup2 old_stdout Unix.stdout;
    Unix.close old_stdout
  with e ->
    Unix.dup2 old_stdout Unix.stdout;
    Unix.close old_stdout;
    raise e

(** Helper: Check that a program raises a specific runtime error *)
let test_raises_error expected_error prog_str () =
  let prog = Parse.program_from_string prog_str in
  (* Redirect stdout to suppress execution trace during tests *)
  let old_stdout = Unix.dup Unix.stdout in
  let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
  Unix.dup2 devnull Unix.stdout;
  Unix.close devnull;
  
  try
    Interpreter.execute_program prog;
    Unix.dup2 old_stdout Unix.stdout;
    Unix.close old_stdout;
    Alcotest.fail "Expected RuntimeError but program succeeded"
  with
  | Interpreter.RuntimeError err ->
      Unix.dup2 old_stdout Unix.stdout;
      Unix.close old_stdout;
      Alcotest.(check bool) "correct error type" true (expected_error err)
  | e ->
      Unix.dup2 old_stdout Unix.stdout;
      Unix.close old_stdout;
      Alcotest.failf "Unexpected exception: %s" (Printexc.to_string e)

(** Test: Simple communication *)
let test_simple_comm () =
  let prog_str = {|
P1 = p2![42].0
P2 = p1?(x).0

main = p1 :: P1 | p2 :: P2
|} in
  test_executes_ok prog_str ()

(** Test: If-then-else with communication *)
let test_if_comm () =
  let prog_str = {|
P1 = if (5 > 3) then p2![1].0 else p2![0].0
P2 = p1?(x).0

main = p1 :: P1 | p2 :: P2
|} in
  test_executes_ok prog_str ()

(** Test: Sequential communication *)
let test_sequence () =
  let prog_str = {|
P1 = p2![10].p2![20].0
P2 = p1?(x).p1?(y).0

main = p1 :: P1 | p2 :: P2
|} in
  test_executes_ok prog_str ()

(** Test: Type error in if condition *)
let test_type_error_if () =
  let prog_str = {|
P = if 42 then p![1].0 else p![0].0

main = client :: P
|} in
  let is_type_error = function
    | Interpreter.TypeError _ -> true
    | _ -> false
  in
  test_raises_error is_type_error prog_str ()

(** Test: Undefined variable *)
let test_undefined_var () =
  let prog_str = {|
P = p![x + 1].0

main = sender :: P
|} in
  let is_undefined_var = function
    | Interpreter.UndefinedVariable _ -> true
    | _ -> false
  in
  test_raises_error is_undefined_var prog_str ()

(** Test: Internal choice with matching external choice *)
let test_internal_choice () =
  let prog_str = {|
Sender = receiver:option1.0
Receiver = sender?{option1: 0, option2: 0, option3: 0}

main = sender :: Sender | receiver :: Receiver
|} in
  test_executes_ok prog_str ()

(** Test: Division by zero *)
let test_division_by_zero () =
  let prog_str = {|
P = p![10 / 0].0

main = sender :: P
|} in
  let is_type_error = function
    | Interpreter.TypeError _ -> true
    | _ -> false
  in
  test_raises_error is_type_error prog_str ()

(** Test: Three-way communication *)
let test_three_way () =
  let prog_str = {|
P1 = p2![10].p3![20].0
P2 = p1?(x).p3![x].0
P3 = p1?(y).p2?(z).0

main = p1 :: P1 | p2 :: P2 | p3 :: P3
|} in
  test_executes_ok prog_str ()

(** Test: Arithmetic expressions *)
let test_arithmetic () =
  let prog_str = {|
P1 = p2![10 + 5 * 2].0
P2 = p1?(x).0

main = p1 :: P1 | p2 :: P2
|} in
  test_executes_ok prog_str ()

(** Test: Boolean expressions *)
let test_boolean () =
  let prog_str = {|
P1 = if ((10 > 5) and (3 < 7)) then p2![true].0 else p2![false].0
P2 = p1?(x).0

main = p1 :: P1 | p2 :: P2
|} in
  test_executes_ok prog_str ()

(** Test: Modulo operation *)
let test_modulo () =
  let prog_str = {|
P1 = p2![17 mod 5].0
P2 = p1?(x).0

main = p1 :: P1 | p2 :: P2
|} in
  test_executes_ok prog_str ()

(** Test suite *)
let suite =
  [
    ("simple communication", `Quick, test_simple_comm);
    ("if-then-else", `Quick, test_if_comm);
    ("sequential communication", `Quick, test_sequence);
    ("type error in if", `Quick, test_type_error_if);
    ("undefined variable", `Quick, test_undefined_var);
    ("internal choice", `Quick, test_internal_choice);
    ("division by zero", `Quick, test_division_by_zero);
    ("three-way communication", `Quick, test_three_way);
    ("arithmetic expressions", `Quick, test_arithmetic);
    ("boolean expressions", `Quick, test_boolean);
    ("modulo operation", `Quick, test_modulo);
  ]

let () =
  Alcotest.run "Interpreter" [
    ("execution", suite);
  ]
