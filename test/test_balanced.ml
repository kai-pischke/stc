(* test_balanced.ml: Test suite for balancedness checking *)

open Stc
open Ast

(* Helper to create a dummy location *)
let loc = Loc.dummy

(* Test 1: Simple balanced protocol - both participants in all branches *)
let test_balanced_simple () =
  (* p -> q: {l1: q -> p: Int; end, l2: q -> p: Int; end} *)
  let g = GBra ("p", "q", [
    ("l1", GMsg ("q", "p", "Int", GEnd loc, loc));
    ("l2", GMsg ("q", "p", "Int", GEnd loc, loc));
  ], loc) in
  Alcotest.(check bool) "balanced simple" true (Balanced.is_balanced g)

(* Test 2: Unbalanced - different participants in branches *)
let test_unbalanced_different_roles () =
  (* p -> q: {l1: q -> r: Int; end, l2: q -> s: Int; end} 
     Unavoidable: {} (r in l1 but not l2, s in l2 but not l1)
     Reachable: {r, s} *)
  let g = GBra ("p", "q", [
    ("l1", GMsg ("q", "r", "Int", GEnd loc, loc));
    ("l2", GMsg ("q", "s", "Int", GEnd loc, loc));
  ], loc) in
  Alcotest.(check bool) "unbalanced different roles" false (Balanced.is_balanced g)

(* Test 3: Balanced - same roles in all branches *)
let test_balanced_same_roles () =
  (* p -> q: {l1: q -> r: Int; end, l2: q -> r: Int; end} *)
  let g = GBra ("p", "q", [
    ("l1", GMsg ("q", "r", "Int", GEnd loc, loc));
    ("l2", GMsg ("q", "r", "Int", GEnd loc, loc));
  ], loc) in
  Alcotest.(check bool) "balanced same roles" true (Balanced.is_balanced g)

(* Test 4: Nested branches - balanced *)
let test_balanced_nested () =
  (* p -> q: {
       l1: q -> r: { k1: end, k2: end },
       l2: q -> r: { k1: end, k2: end }
     } *)
  let inner1 = GBra ("q", "r", [
    ("k1", GEnd loc);
    ("k2", GEnd loc);
  ], loc) in
  let inner2 = GBra ("q", "r", [
    ("k1", GEnd loc);
    ("k2", GEnd loc);
  ], loc) in
  let g = GBra ("p", "q", [
    ("l1", inner1);
    ("l2", inner2);
  ], loc) in
  Alcotest.(check bool) "balanced nested" true (Balanced.is_balanced g)

(* Test 5: Nested branches - unbalanced inner *)
let test_unbalanced_nested () =
  (* p -> q: {
       l1: q -> r: { k1: r -> s: end, k2: end },  (* inner unbalanced *)
       l2: q -> r: { k1: r -> s: end, k2: end }
     } *)
  let inner1 = GBra ("q", "r", [
    ("k1", GMsg ("r", "s", "Int", GEnd loc, loc));
    ("k2", GEnd loc);
  ], loc) in
  let inner2 = GBra ("q", "r", [
    ("k1", GMsg ("r", "s", "Int", GEnd loc, loc));
    ("k2", GEnd loc);
  ], loc) in
  let g = GBra ("p", "q", [
    ("l1", inner1);
    ("l2", inner2);
  ], loc) in
  Alcotest.(check bool) "unbalanced nested" false (Balanced.is_balanced g)

(* Test 6: Parallel composition - balanced *)
let test_balanced_parallel () =
  (* (p -> q: Int; end) | (r -> s: Int; end) *)
  let g1 = GMsg ("p", "q", "Int", GEnd loc, loc) in
  let g2 = GMsg ("r", "s", "Int", GEnd loc, loc) in
  let g = GPar (g1, g2, loc) in
  Alcotest.(check bool) "balanced parallel" true (Balanced.is_balanced g)

(* Test 7: Recursion - balanced *)
let test_balanced_recursion () =
  (* rec T. p -> q: {l1: T, l2: end} *)
  let body = GBra ("p", "q", [
    ("l1", GVar ("T", loc));
    ("l2", GEnd loc);
  ], loc) in
  let g = GRec ("T", body, loc) in
  Alcotest.(check bool) "balanced recursion" true (Balanced.is_balanced g)

(* Test 8: Three branches - all have same participants *)
let test_balanced_three_branches () =
  (* p -> q: {l1: q -> r: end, l2: q -> r: end, l3: q -> r: end} *)
  let g = GBra ("p", "q", [
    ("l1", GMsg ("q", "r", "Int", GEnd loc, loc));
    ("l2", GMsg ("q", "r", "Int", GEnd loc, loc));
    ("l3", GMsg ("q", "r", "Int", GEnd loc, loc));
  ], loc) in
  Alcotest.(check bool) "balanced three branches" true (Balanced.is_balanced g)

(* Test 9: Three branches - one differs *)
let test_unbalanced_three_branches () =
  (* p -> q: {l1: q -> r: end, l2: q -> r: end, l3: q -> s: end} *)
  let g = GBra ("p", "q", [
    ("l1", GMsg ("q", "r", "Int", GEnd loc, loc));
    ("l2", GMsg ("q", "r", "Int", GEnd loc, loc));
    ("l3", GMsg ("q", "s", "Int", GEnd loc, loc));
  ], loc) in
  Alcotest.(check bool) "unbalanced three branches" false (Balanced.is_balanced g)

(* Test 10: Check detailed error message *)
let test_error_message () =
  let g = GBra ("p", "q", [
    ("l1", GMsg ("q", "r", "Int", GEnd loc, loc));
    ("l2", GMsg ("q", "s", "Int", GEnd loc, loc));
  ], loc) in
  let (is_bal, msg) = Balanced.check_balanced g in
  Alcotest.(check bool) "returns false" false is_bal;
  Alcotest.(check bool) "has error message" true (Option.is_some msg)

(* Test 11: Multiple participants in branches - balanced *)
let test_balanced_multiple_participants () =
  (* p -> q: {
       l1: q -> r: Int; r -> s: Int; end,
       l2: q -> r: Int; r -> s: Int; end
     } *)
  let branch1 = GMsg ("q", "r", "Int", 
                      GMsg ("r", "s", "Int", GEnd loc, loc), loc) in
  let branch2 = GMsg ("q", "r", "Int", 
                      GMsg ("r", "s", "Int", GEnd loc, loc), loc) in
  let g = GBra ("p", "q", [
    ("l1", branch1);
    ("l2", branch2);
  ], loc) in
  Alcotest.(check bool) "balanced multiple participants" true (Balanced.is_balanced g)

(* Test 12: Multiple participants - one missing in one branch *)
let test_unbalanced_missing_participant () =
  (* p -> q: {
       l1: q -> r: Int; r -> s: Int; end,
       l2: q -> r: Int; end   (* s missing here *)
     } *)
  let branch1 = GMsg ("q", "r", "Int", 
                      GMsg ("r", "s", "Int", GEnd loc, loc), loc) in
  let branch2 = GMsg ("q", "r", "Int", GEnd loc, loc) in
  let g = GBra ("p", "q", [
    ("l1", branch1);
    ("l2", branch2);
  ], loc) in
  Alcotest.(check bool) "unbalanced missing participant" false (Balanced.is_balanced g)

(* Test suite *)
let () =
  let open Alcotest in
  run "Balancedness Tests" [
    "Basic Cases", [
      test_case "balanced simple" `Quick test_balanced_simple;
      test_case "balanced same roles" `Quick test_balanced_same_roles;
      test_case "unbalanced different roles" `Quick test_unbalanced_different_roles;
    ];
    "Nested Branches", [
      test_case "balanced nested" `Quick test_balanced_nested;
      test_case "unbalanced nested" `Quick test_unbalanced_nested;
    ];
    "Parallel and Recursion", [
      test_case "balanced parallel" `Quick test_balanced_parallel;
      test_case "balanced recursion" `Quick test_balanced_recursion;
    ];
    "Multiple Branches", [
      test_case "balanced three branches" `Quick test_balanced_three_branches;
      test_case "unbalanced three branches" `Quick test_unbalanced_three_branches;
    ];
    "Error Messages", [
      test_case "error message" `Quick test_error_message;
    ];
    "Multiple Participants", [
      test_case "balanced multiple participants" `Quick test_balanced_multiple_participants;
      test_case "unbalanced missing participant" `Quick test_unbalanced_missing_participant;
    ];
  ]

