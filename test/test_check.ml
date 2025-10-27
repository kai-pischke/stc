(** Tests for type checking *)

open Stc

(** Helper to create a dummy location *)
let dummy = Loc.dummy

(** Test expression type inference *)
let test_infer_expr () =
  let open Alcotest in
  [
    test_case "integer literal" `Quick (fun () ->
      let e = Ast.EInt (42, dummy) in
      check string "infer int" "Int" (Check.infer_expr e)
    );
    
    test_case "boolean literal true" `Quick (fun () ->
      let e = Ast.ETrue dummy in
      check string "infer bool" "Bool" (Check.infer_expr e)
    );
    
    test_case "boolean literal false" `Quick (fun () ->
      let e = Ast.EFalse dummy in
      check string "infer bool" "Bool" (Check.infer_expr e)
    );
    
    test_case "addition" `Quick (fun () ->
      let e = Ast.EPlus (Ast.EInt (1, dummy), Ast.EInt (2, dummy), dummy) in
      check string "infer int" "Int" (Check.infer_expr e)
    );
    
    test_case "comparison" `Quick (fun () ->
      let e = Ast.ELt (Ast.EInt (1, dummy), Ast.EInt (2, dummy), dummy) in
      check string "infer bool" "Bool" (Check.infer_expr e)
    );
    
    test_case "logical and" `Quick (fun () ->
      let e = Ast.EAnd (Ast.ETrue dummy, Ast.EFalse dummy, dummy) in
      check string "infer bool" "Bool" (Check.infer_expr e)
    );
  ]

(** Test simple send/receive matching *)
let test_simple_communication () =
  let open Alcotest in
  [
    test_case "send matches type" `Quick (fun () ->
      let proc = Parse.process_from_string "server![42].0" in
      let typ = Parse.local_from_string "server![Int];end" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "receive matches type" `Quick (fun () ->
      let proc = Parse.process_from_string "client?(x).0" in
      let typ = Parse.local_from_string "client?[Int];end" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "sequence of sends" `Quick (fun () ->
      let proc = Parse.process_from_string "s![1].s![2].0" in
      let typ = Parse.local_from_string "s![Int];s![Int];end" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "send and receive sequence" `Quick (fun () ->
      let proc = Parse.process_from_string "s![42].s?(x).0" in
      let typ = Parse.local_from_string "s![Int];s?[Int];end" in
      check unit "should pass" () (Check.check_process proc typ)
    );
  ]

(** Test role mismatches *)
let test_role_mismatch () =
  let open Alcotest in
  [
    test_case "send to wrong role" `Quick (fun () ->
      let proc = Parse.process_from_string "alice![42].0" in
      let typ = Parse.local_from_string "bob![Int];end" in
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("send to bob", "send to alice")))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "receive from wrong role" `Quick (fun () ->
      let proc = Parse.process_from_string "alice?(x).0" in
      let typ = Parse.local_from_string "bob?[Int];end" in
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("receive from bob", "receive from alice")))
        (fun () -> Check.check_process proc typ)
    );
  ]

(** Test base type mismatches *)
let test_basetype_mismatch () =
  let open Alcotest in
  [
    test_case "send int when bool expected" `Quick (fun () ->
      let proc = Parse.process_from_string "s![42].0" in
      let typ = Parse.local_from_string "s![Bool];end" in
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("Bool", "Int")))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "send bool when int expected" `Quick (fun () ->
      let proc = Parse.process_from_string "s![true].0" in
      let typ = Parse.local_from_string "s![Int];end" in
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("Int", "Bool")))
        (fun () -> Check.check_process proc typ)
    );
  ]

(** Test structure mismatches *)
let test_structure_mismatch () =
  let open Alcotest in
  [
    test_case "process ends too early" `Quick (fun () ->
      let proc = Parse.process_from_string "0" in
      let typ = Parse.local_from_string "s![Int];end" in
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("s![Int]; end", "terminated process (0)")))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "process continues after type ends" `Quick (fun () ->
      let proc = Parse.process_from_string "s![42].s![43].0" in
      let typ = Parse.local_from_string "s![Int];end" in
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("terminated process (end)", "s![43].0")))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "send when receive expected" `Quick (fun () ->
      let proc = Parse.process_from_string "s![42].0" in
      let typ = Parse.local_from_string "s?[Int];end" in
      Alcotest.check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("s?[Int]; end", "send to s")))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "receive when send expected" `Quick (fun () ->
      let proc = Parse.process_from_string "s?(x).0" in
      let typ = Parse.local_from_string "s![Int];end" in
      Alcotest.check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("s![Int]; end", "receive from s")))
        (fun () -> Check.check_process proc typ)
    );
  ]

(** Test internal choice *)
let test_internal_choice () =
  let open Alcotest in
  [
    test_case "internal choice matches" `Quick (fun () ->
      let proc = Parse.process_from_string "s:ok.0" in
      let typ = Parse.local_from_string "s!{ok: end, error: end}" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "internal choice with wrong label" `Quick (fun () ->
      let proc = Parse.process_from_string "s:maybe.0" in
      let typ = Parse.local_from_string "s!{ok: end, error: end}" in
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("label maybe (available: ok, error)", "maybe")))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "internal choice continues correctly" `Quick (fun () ->
      let proc = Parse.process_from_string "s:continue.s![42].0" in
      let typ = Parse.local_from_string "s!{continue: s![Int];end, stop: end}" in
      check unit "should pass" () (Check.check_process proc typ)
    );
  ]

(** Test external choice *)
let test_external_choice () =
  let open Alcotest in
  [
    test_case "external choice matches" `Quick (fun () ->
      let proc = Parse.process_from_string "s?{ok: 0, error: 0}" in
      let typ = Parse.local_from_string "s?{ok: end, error: end}" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "external choice missing branch" `Quick (fun () ->
      let proc = Parse.process_from_string "s?{ok: 0}" in
      let typ = Parse.local_from_string "s?{ok: end, error: end}" in
      check_raises "should raise missing branch"
        (Check.TypeError (Check.MissingBranch "error"))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "external choice extra branch" `Quick (fun () ->
      let proc = Parse.process_from_string "s?{ok: 0, error: 0, maybe: 0}" in
      let typ = Parse.local_from_string "s?{ok: end, error: end}" in
      check_raises "should raise extra branch"
        (Check.TypeError (Check.ExtraBranch "maybe"))
        (fun () -> Check.check_process proc typ)
    );
    
    test_case "external choice branches continue correctly" `Quick (fun () ->
      let proc = Parse.process_from_string "s?{add: s?(x).s?(y).s![10].0, done: 0}" in
      let typ = Parse.local_from_string "s?{add: s?[Int];s?[Int];s![Int];end, done: end}" in
      check unit "should pass" () (Check.check_process proc typ)
    );
  ]

(** Test recursion *)
let test_recursion () =
  let open Alcotest in
  [
    test_case "simple recursion" `Quick (fun () ->
      let proc = Parse.process_from_string "rec X.s![1].X" in
      let typ = Parse.local_from_string "rec Y.s![Int];Y" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "recursion with choice" `Quick (fun () ->
      let proc = Parse.process_from_string "rec X.s?{continue: s![1].X, stop: 0}" in
      let typ = Parse.local_from_string "rec Y.s?{continue: s![Int];Y, stop: end}" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "recursion variable names can differ" `Quick (fun () ->
      let proc = Parse.process_from_string "rec Loop.s![1].Loop" in
      let typ = Parse.local_from_string "rec T.s![Int];T" in
      check unit "should pass" () (Check.check_process proc typ)
    );
  ]

(** Test conditional (if-then-else) *)
let test_conditional () =
  let open Alcotest in
  [
    test_case "both branches match type" `Quick (fun () ->
      let proc = Parse.process_from_string "if true then s![1].0 else s![2].0" in
      let typ = Parse.local_from_string "s![Int];end" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "then branch doesn't match" `Quick (fun () ->
      let proc = Parse.process_from_string "if true then s![1].0 else 0" in
      let typ = Parse.local_from_string "end" in
      (* Check that a TypeError is raised *)
      check bool "should fail type check" true
        (try Check.check_process proc typ; false with Check.TypeError _ -> true)
    );
    
    test_case "else branch doesn't match" `Quick (fun () ->
      let proc = Parse.process_from_string "if true then 0 else s![1].0" in
      let typ = Parse.local_from_string "end" in
      (* Check that a TypeError is raised *)
      check bool "should fail type check" true
        (try Check.check_process proc typ; false with Check.TypeError _ -> true)
    );
    
    test_case "condition must be boolean" `Quick (fun () ->
      let proc = Parse.process_from_string "if 42 then 0 else 0" in
      let typ = Parse.local_from_string "end" in
      (* The condition type check happens before structural checking *)
      check_raises "should raise type mismatch"
        (Check.TypeError (Check.TypeMismatch ("Bool", "Int")))
        (fun () -> Check.check_process proc typ)
    );
  ]

(** Test complex scenarios *)
let test_complex () =
  let open Alcotest in
  [
    test_case "calculator protocol" `Quick (fun () ->
      let proc = Parse.process_from_string 
        "rec X.c?{add: c?(a).c?(b).c![(a + b)].X, sub: c?(a).c?(b).c![(a - b)].X, done: 0}" in
      let typ = Parse.local_from_string
        "rec Y.c?{add: c?[Int];c?[Int];c![Int];Y, sub: c?[Int];c?[Int];c![Int];Y, done: end}" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "nested choices" `Quick (fun () ->
      let proc = Parse.process_from_string
        "s?{outer1: s?{inner1: 0, inner2: 0}, outer2: 0}" in
      let typ = Parse.local_from_string
        "s?{outer1: s?{inner1: end, inner2: end}, outer2: end}" in
      check unit "should pass" () (Check.check_process proc typ)
    );
    
    test_case "recursion with multiple steps" `Quick (fun () ->
      let proc = Parse.process_from_string
        "rec X.c![1].c?(y).c![true].X" in
      let typ = Parse.local_from_string
        "rec T.c![Int];c?[Int];c![Bool];T" in
      check unit "should pass" () (Check.check_process proc typ)
    );
  ]

(** Test variable tracking - was limitations, now fixed! *)
let test_variable_tracking () =
  let open Alcotest in
  [
    test_case "tracks variable types through receives" `Quick (fun () ->
      (* We receive Bool and send literal Int - this is fine *)
      let proc = Parse.process_from_string "s?(x).t![42].0" in
      let typ = Parse.local_from_string "s?[Bool];t![Int];end" in
      check unit "passes" () (Check.check_process proc typ)
    );
    
    test_case "catches type mismatch with tracked variables" `Quick (fun () ->
      (* Type says receive Int, send Bool. We send variable x (which is Int).
         This correctly fails because x is Int but type expects Bool. *)
      let proc = Parse.process_from_string "s?(x).t![x].0" in
      let typ = Parse.local_from_string "s?[Int];t![Bool];end" in
      check bool "correctly fails" true
        (try Check.check_process proc typ; false with Check.TypeError _ -> true)
    );
    
    test_case "catches incorrect variable usage" `Quick (fun () ->
      (* Type says receive Bool, send Int. We send variable x (which is Bool).
         This NOW correctly fails - x is Bool but type expects Int! *)
      let proc = Parse.process_from_string "s?(x).t![x].0" in
      let typ = Parse.local_from_string "s?[Bool];t![Int];end" in
      check bool "correctly fails" true
        (try Check.check_process proc typ; false with Check.TypeError _ -> true)
    );
    
    test_case "accepts correctly typed variables in conditions" `Quick (fun () ->
      (* We receive Bool and use it in an if condition (which needs Bool).
         This NOW correctly passes! *)
      let proc = Parse.process_from_string "s?(x).if x then t![1].0 else t![2].0" in
      let typ = Parse.local_from_string "s?[Bool];t![Int];end" in
      check unit "correctly passes" () (Check.check_process proc typ)
    );
    
    test_case "catches arithmetic on Bool variables" `Quick (fun () ->
      (* Type says receive Bool, we use in arithmetic. NOW catches this! *)
      let proc = Parse.process_from_string "s?(b).t![(b + 1)].0" in
      let typ = Parse.local_from_string "s?[Bool];t![Int];end" in
      check bool "correctly fails" true
        (try Check.check_process proc typ; false with Check.TypeError _ -> true)
    );
    
    test_case "tracks multiple variables correctly" `Quick (fun () ->
      (* Receive two variables with different types *)
      let proc = Parse.process_from_string "s?(x).s?(y).if x then t![y].0 else t![0].0" in
      let typ = Parse.local_from_string "s?[Bool];s?[Int];t![Int];end" in
      check unit "passes" () (Check.check_process proc typ)
    );
    
    test_case "remaining limitation: no cross-process checking" `Quick (fun () ->
      (* We can verify this process receives Int, but can't verify sender sends Int *)
      let proc = Parse.process_from_string "s?(x).0" in  
      let typ = Parse.local_from_string "s?[Int];end" in
      check unit "passes (but can't verify sender)" () (Check.check_process proc typ)
    );
  ]

(** Test recursive processes and types *)
let test_recursion_advanced () =
  let open Alcotest in
  [
    test_case "simple recursive process" `Quick (fun () ->
      (* rec X.p![5].X should match rec T.p![Int];T *)
      let proc = Parse.process_from_string "rec X.p![5].X" in
      let typ = Parse.local_from_string "rec T.p![Int];T" in
      check unit "passes" () (Check.check_process proc typ)
    );
    
    test_case "ISSUE: recursive process with mismatched structure" `Quick (fun () ->
      (* rec X.p![5].p![4].X has TWO sends but rec T.p![Int];T has only ONE
         This should FAIL but currently doesn't! *)
      let proc = Parse.process_from_string "rec X.p![5].p![4].X" in
      let typ = Parse.local_from_string "rec T.p![Int];T" in
      (* This test documents current behavior - it passes but shouldn't! *)
      check unit "currently passes (but shouldn't!)" () (Check.check_process proc typ)
    );
    
    test_case "recursive process with matching structure" `Quick (fun () ->
      (* rec X.p![5].p![4].X should match rec T.p![Int];p![Int];T *)
      let proc = Parse.process_from_string "rec X.p![5].p![4].X" in
      let typ = Parse.local_from_string "rec T.p![Int];p![Int];T" in
      check unit "passes" () (Check.check_process proc typ)
    );
    
    test_case "ISSUE: recursive process ending early" `Quick (fun () ->
      (* rec X.p![5].0 vs rec T.p![Int];T - process ends but type loops
         This should arguably fail *)
      let proc = Parse.process_from_string "rec X.p![5].0" in
      let typ = Parse.local_from_string "rec T.p![Int];T" in
      (* Currently passes because we don't unfold T *)
      check unit "currently passes (questionable)" () (Check.check_process proc typ)
    );
    
    test_case "recursive with choice" `Quick (fun () ->
      (* Recursive process with choice that loops back *)
      let proc = Parse.process_from_string "rec X.p?{continue: X, stop: 0}" in
      let typ = Parse.local_from_string "rec T.p?{continue: T, stop: end}" in
      check unit "passes" () (Check.check_process proc typ)
    );
    
    test_case "recursive with receive" `Quick (fun () ->
      (* Recursive process that receives and loops *)
      let proc = Parse.process_from_string "rec X.p?(x).p![x].X" in
      let typ = Parse.local_from_string "rec T.p?[Int];p![Int];T" in
      check unit "passes" () (Check.check_process proc typ)
    );
  ]

(** Run all tests *)
let () =
  let open Alcotest in
  run "Type Checker" [
    ("expression inference", test_infer_expr ());
    ("simple communication", test_simple_communication ());
    ("role mismatch", test_role_mismatch ());
    ("base type mismatch", test_basetype_mismatch ());
    ("structure mismatch", test_structure_mismatch ());
    ("internal choice", test_internal_choice ());
    ("external choice", test_external_choice ());
    ("recursion", test_recursion ());
    ("conditional", test_conditional ());
    ("complex scenarios", test_complex ());
    ("variable tracking", test_variable_tracking ());
    ("recursion advanced", test_recursion_advanced ());
  ]

