(** Tests for session type subtyping *)

open Stc

(** Test basic structural rules *)
let test_basic () =
  let open Alcotest in
  [
    test_case "end <= end" `Quick (fun () ->
      let t1 = Parse.local_from_string "end" in
      let t2 = Parse.local_from_string "end" in
      check bool "should be subtype" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p![Int];end <= p![Int];end" `Quick (fun () ->
      let t1 = Parse.local_from_string "p![Int]; end" in
      let t2 = Parse.local_from_string "p![Int]; end" in
      check bool "should be subtype" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p?[Int];end <= p?[Int];end" `Quick (fun () ->
      let t1 = Parse.local_from_string "p?[Int]; end" in
      let t2 = Parse.local_from_string "p?[Int]; end" in
      check bool "should be subtype" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p![Int];end NOT<= p![Bool];end" `Quick (fun () ->
      let t1 = Parse.local_from_string "p![Int]; end" in
      let t2 = Parse.local_from_string "p![Bool]; end" in
      check bool "should not be subtype" false (Subtype.is_subtype t1 t2)
    );
    
    test_case "p![Int];end NOT<= q![Int];end" `Quick (fun () ->
      let t1 = Parse.local_from_string "p![Int]; end" in
      let t2 = Parse.local_from_string "q![Int]; end" in
      check bool "should not be subtype" false (Subtype.is_subtype t1 t2)
    );
  ]

(** Test internal choice covariance *)
let test_internal_choice () =
  let open Alcotest in
  [
    test_case "p!{l1:end, l2:end} <= p!{l1:end, l2:end, l3:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p!{l1: end, l2: end}" in
      let t2 = Parse.local_from_string "p!{l1: end, l2: end, l3: end}" in
      check bool "covariant - fewer options is subtype" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p!{l1:end} <= p!{l1:end, l2:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p!{l1: end}" in
      let t2 = Parse.local_from_string "p!{l1: end, l2: end}" in
      check bool "covariant - single label <= multiple" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p!{l1:end, l2:end, l3:end} NOT<= p!{l1:end, l2:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p!{l1: end, l2: end, l3: end}" in
      let t2 = Parse.local_from_string "p!{l1: end, l2: end}" in
      check bool "more options not subtype of fewer" false (Subtype.is_subtype t1 t2)
    );
    
    test_case "p!{l1:end, l2:end} NOT<= p!{l1:end, l3:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p!{l1: end, l2: end}" in
      let t2 = Parse.local_from_string "p!{l1: end, l3: end}" in
      check bool "missing label in t2" false (Subtype.is_subtype t1 t2)
    );
  ]

(** Test external choice contravariance *)
let test_external_choice () =
  let open Alcotest in
  [
    test_case "p?{l1:end, l2:end, l3:end} <= p?{l1:end, l2:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p?{l1: end, l2: end, l3: end}" in
      let t2 = Parse.local_from_string "p?{l1: end, l2: end}" in
      check bool "contravariant - more options is subtype" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p?{l1:end, l2:end} <= p?{l1:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p?{l1: end, l2: end}" in
      let t2 = Parse.local_from_string "p?{l1: end}" in
      check bool "contravariant - multiple <= single" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p?{l1:end, l2:end} NOT<= p?{l1:end, l2:end, l3:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p?{l1: end, l2: end}" in
      let t2 = Parse.local_from_string "p?{l1: end, l2: end, l3: end}" in
      check bool "fewer options not subtype of more" false (Subtype.is_subtype t1 t2)
    );
    
    test_case "p?{l1:end, l2:end} NOT<= p?{l1:end, l3:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p?{l1: end, l2: end}" in
      let t2 = Parse.local_from_string "p?{l1: end, l3: end}" in
      check bool "missing label in t1" false (Subtype.is_subtype t1 t2)
    );
  ]

(** Test label permutations *)
let test_label_permutations () =
  let open Alcotest in
  [
    test_case "p!{l1:end, l2:end} = p!{l2:end, l1:end} (permuted)" `Quick (fun () ->
      let t1 = Parse.local_from_string "p!{l1: end, l2: end}" in
      let t2 = Parse.local_from_string "p!{l2: end, l1: end}" in
      check bool "permuted labels are equal" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p!{l1:end, l2:end} = p!{l2:end, l1:end} (reverse)" `Quick (fun () ->
      let t2 = Parse.local_from_string "p!{l1: end, l2: end}" in
      let t1 = Parse.local_from_string "p!{l2: end, l1: end}" in
      check bool "reverse check also works" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p?{a:end, b:end, c:end} = p?{c:end, a:end, b:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p?{a: end, b: end, c: end}" in
      let t2 = Parse.local_from_string "p?{c: end, a: end, b: end}" in
      check bool "external choice permutation" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p!{l1:end, l2:end, l3:end} <= p!{l3:end, l2:end, l1:end, l4:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p!{l1: end, l2: end, l3: end}" in
      let t2 = Parse.local_from_string "p!{l3: end, l2: end, l1: end, l4: end}" in
      check bool "covariance with permutation" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p?{l1:end, l2:end, l3:end, l4:end} <= p?{l3:end, l1:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "p?{l1: end, l2: end, l3: end, l4: end}" in
      let t2 = Parse.local_from_string "p?{l3: end, l1: end}" in
      check bool "contravariance with permutation" true (Subtype.is_subtype t1 t2)
    );
  ]

(** Test simple recursion *)
let test_simple_recursion () =
  let open Alcotest in
  [
    test_case "rec T. p![Int]; T = rec T. p![Int]; T" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p![Int]; T" in
      let t2 = Parse.local_from_string "rec T. p![Int]; T" in
      check bool "same recursive type" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. end = end" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. end" in
      let t2 = Parse.local_from_string "end" in
      check bool "trivial recursion = base case" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "end = rec T. end" `Quick (fun () ->
      let t1 = Parse.local_from_string "end" in
      let t2 = Parse.local_from_string "rec T. end" in
      check bool "base case = trivial recursion" true (Subtype.is_subtype t1 t2)
    );
  ]

(** Test different unfolding periods 
    
    Note: With coinductive subtyping, types with different periods
    CAN be subtypes of each other! This is correct behavior for
    infinite unfoldings. The coinductive assumption allows us to
    prove the relation holds.
*)
let test_unfolding_periods () =
  let open Alcotest in
  [
    test_case "rec T. p![Int]; T <= rec T. p![Int]; p![Int]; T (coinductive)" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p![Int]; T" in
      let t2 = Parse.local_from_string "rec T. p![Int]; p![Int]; T" in
      check bool "period 1 is subtype of period 2 (coinductively)" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. p![Int]; p![Int]; T <= rec T. p![Int]; T (coinductive)" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p![Int]; p![Int]; T" in
      let t2 = Parse.local_from_string "rec T. p![Int]; T" in
      check bool "period 2 is subtype of period 1 (coinductively)" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. p![Int]; p![Int]; p![Int]; T <= rec T. p![Int]; T" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p![Int]; p![Int]; p![Int]; T" in
      let t2 = Parse.local_from_string "rec T. p![Int]; T" in
      check bool "period 3 is subtype of period 1 (coinductively)" true (Subtype.is_subtype t1 t2)
    );
  ]

(** Test extra unused recursion binders *)
let test_unused_binders () =
  let open Alcotest in
  [
    test_case "rec T. p![Int]; end = p![Int]; end" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p![Int]; end" in
      let t2 = Parse.local_from_string "p![Int]; end" in
      check bool "unused binder eliminates" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "p![Int]; end = rec T. p![Int]; end" `Quick (fun () ->
      let t1 = Parse.local_from_string "p![Int]; end" in
      let t2 = Parse.local_from_string "rec T. p![Int]; end" in
      check bool "can introduce unused binder" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. rec S. p![Int]; end = p![Int]; end" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. rec S. p![Int]; end" in
      let t2 = Parse.local_from_string "p![Int]; end" in
      check bool "multiple unused binders" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. rec S. p![Int]; S NOT= rec T. p![Int]; T" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. rec S. p![Int]; S" in
      let t2 = Parse.local_from_string "rec T. p![Int]; T" in
      check bool "inner vs outer binder usage" true (Subtype.is_subtype t1 t2)
    );
  ]

(** Test combinations of recursion with co/contravariance *)
let test_recursion_with_variance () =
  let open Alcotest in
  [
    test_case "rec T. p!{l1:T, l2:end} <= rec T. p!{l1:T, l2:end, l3:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p!{l1: T, l2: end}" in
      let t2 = Parse.local_from_string "rec T. p!{l1: T, l2: end, l3: end}" in
      check bool "recursive with internal choice covariance" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. p?{l1:T, l2:end, l3:end} <= rec T. p?{l1:T, l2:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p?{l1: T, l2: end, l3: end}" in
      let t2 = Parse.local_from_string "rec T. p?{l1: T, l2: end}" in
      check bool "recursive with external choice contravariance" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. p!{a:T, b:end} <= rec S. p!{b:end, a:S, c:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p!{a: T, b: end}" in
      let t2 = Parse.local_from_string "rec S. p!{b: end, a: S, c: end}" in
      check bool "different binder names + permutation + covariance" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. p?{x:T, y:end, z:end} <= rec S. p?{z:end, x:S}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p?{x: T, y: end, z: end}" in
      let t2 = Parse.local_from_string "rec S. p?{z: end, x: S}" in
      check bool "different names + permutation + contravariance" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. p![Int]; p!{l1:T, l2:end} <= rec T. p![Int]; p!{l1:T, l2:end, l3:end}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p![Int]; p!{l1: T, l2: end}" in
      let t2 = Parse.local_from_string "rec T. p![Int]; p!{l1: T, l2: end, l3: end}" in
      check bool "prefix + recursive choice covariance" true (Subtype.is_subtype t1 t2)
    );
  ]

(** Test nested recursion *)
let test_nested_recursion () =
  let open Alcotest in
  [
    test_case "rec T. p![Int]; T = rec T. p![Int]; T (basic)" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p![Int]; T" in
      let t2 = Parse.local_from_string "rec T. p![Int]; T" in
      check bool "nested recursion equality" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "rec T. rec S. p!{l1:S, l2:T} = rec T. rec S. p!{l2:T, l1:S}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. rec S. p!{l1: S, l2: T}" in
      let t2 = Parse.local_from_string "rec T. rec S. p!{l2: T, l1: S}" in
      check bool "nested with permuted labels" true (Subtype.is_subtype t1 t2)
    );
  ]

(** Test complex mixed cases *)
let test_complex_mixed () =
  let open Alcotest in
  [
    test_case "complex: rec T. p!{a:q![Int];T, b:end} <= rec S. p!{c:end, b:end, a:q![Int];S}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p!{a: q![Int]; T, b: end}" in
      let t2 = Parse.local_from_string "rec S. p!{c: end, b: end, a: q![Int]; S}" in
      check bool "recursion + covariance + permutation + continuation" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "complex: rec T. p?{x:q?[Bool];T, y:end, z:end} <= rec S. p?{z:end, x:q?[Bool];S}" `Quick (fun () ->
      let t1 = Parse.local_from_string "rec T. p?{x: q?[Bool]; T, y: end, z: end}" in
      let t2 = Parse.local_from_string "rec S. p?{z: end, x: q?[Bool]; S}" in
      check bool "recursion + contravariance + permutation + continuation" true (Subtype.is_subtype t1 t2)
    );
    
    test_case "complex: mixed internal and external" `Quick (fun () ->
      (* Internal choice is covariant: fewer options in t1 is ok
         But nested external choice is contravariant: t1 needs MORE options than t2
         So t1 needs to have all labels that appear in t2's external choice *)
      let t1 = Parse.local_from_string "rec T. p!{a: q?{x: T, y: end, z: end}, b: end}" in
      let t2 = Parse.local_from_string "rec S. p!{a: q?{x: S, y: end}, b: end, c: end}" in
      check bool "internal covariant, nested external contravariant" true (Subtype.is_subtype t1 t2)
    );
  ]

(** Run all tests *)
let () =
  Alcotest.run "Subtyping Tests" [
    ("Basic", test_basic ());
    ("Internal Choice (Covariant)", test_internal_choice ());
    ("External Choice (Contravariant)", test_external_choice ());
    ("Label Permutations", test_label_permutations ());
    ("Simple Recursion", test_simple_recursion ());
    ("Unfolding Periods", test_unfolding_periods ());
    ("Unused Binders", test_unused_binders ());
    ("Recursion with Variance", test_recursion_with_variance ());
    ("Nested Recursion", test_nested_recursion ());
    ("Complex Mixed", test_complex_mixed ());
  ]

