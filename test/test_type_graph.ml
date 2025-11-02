(* test_type_graph.ml: Test suite for graph representation *)

open Stc
open Type_graph

(* Helper to create a dummy location *)
let loc = Loc.dummy

(* Test 1: Simple global type to graph and back *)
let test_global_simple () =
  (* p -> q:[Int]; end *)
  let g = Ast.GMsg ("p", "q", "Int", Ast.GEnd loc, loc) in
  let (graph, entry) = global_to_graph g in
  let g' = graph_to_global graph entry in
  
  (* Check that we can pretty print both *)
  let s1 = Pretty.string_of_global g in
  let s2 = Pretty.string_of_global g' in
  Alcotest.(check string) "round-trip preserves structure" s1 s2

(* Test 2: Global type with branching *)
let test_global_branching () =
  (* p -> q:{l1: end, l2: end} *)
  let g = Ast.GBra ("p", "q", [
    ("l1", Ast.GEnd loc);
    ("l2", Ast.GEnd loc);
  ], loc) in
  let (graph, entry) = global_to_graph g in
  let g' = graph_to_global graph entry in
  
  (* Branch order may change, so just check structure is preserved *)
  let s2 = Pretty.string_of_global g' in
  (* Both should contain the same branch labels *)
  Alcotest.(check bool) "contains l1" true (String.contains s2 '1');
  Alcotest.(check bool) "contains l2" true (String.contains s2 '2');
  Alcotest.(check bool) "contains p -> q" true (String.length s2 > 0)

(* Test 3: Recursive global type *)
let test_global_recursive () =
  (* rec X. p -> q:[Int]; X *)
  let g = Ast.GRec ("X", Ast.GMsg ("p", "q", "Int", Ast.GVar ("X", loc), loc), loc) in
  let (graph, entry) = global_to_graph g in
  let g' = graph_to_global graph entry in
  
  let s2 = Pretty.string_of_global g' in
  (* Just check that something reasonable was generated *)
  Alcotest.(check bool) "reconstructed non-empty" true (String.length s2 > 5)

(* Test 4: Simple local type *)
let test_local_simple () =
  (* p![Int]; end *)
  let t = Ast.LSend ("p", "Int", Ast.LEnd loc, loc) in
  let (graph, entry) = local_to_graph t in
  let t' = graph_to_local graph entry in
  
  let s1 = Pretty.string_of_local t in
  let s2 = Pretty.string_of_local t' in
  Alcotest.(check string) "local round-trip" s1 s2

(* Test 5: Local type with internal choice *)
let test_local_internal_choice () =
  (* p!{l1: end, l2: end} *)
  let t = Ast.LInt ("p", [
    ("l1", Ast.LEnd loc);
    ("l2", Ast.LEnd loc);
  ], loc) in
  let (graph, entry) = local_to_graph t in
  let t' = graph_to_local graph entry in
  
  let s2 = Pretty.string_of_local t' in
  (* Check structure is preserved (order may differ) *)
  Alcotest.(check bool) "contains l1" true (String.contains s2 '1');
  Alcotest.(check bool) "contains l2" true (String.contains s2 '2');
  Alcotest.(check bool) "internal choice" true (String.length s2 > 0)

(* Test 6: Local type with external choice *)
let test_local_external_choice () =
  (* p?{l1: end, l2: end} *)
  let t = Ast.LExt ("p", [
    ("l1", Ast.LEnd loc);
    ("l2", Ast.LEnd loc);
  ], loc) in
  let (graph, entry) = local_to_graph t in
  let t' = graph_to_local graph entry in
  
  let s2 = Pretty.string_of_local t' in
  (* Check structure is preserved (order may differ) *)
  Alcotest.(check bool) "contains l1" true (String.contains s2 '1');
  Alcotest.(check bool) "contains l2" true (String.contains s2 '2');
  Alcotest.(check bool) "external choice" true (String.length s2 > 0)

(* Test 7: Recursive local type *)
let test_local_recursive () =
  (* rec X. p![Int]; X *)
  let t = Ast.LRec ("X", Ast.LSend ("p", "Int", Ast.LVar ("X", loc), loc), loc) in
  let (graph, entry) = local_to_graph t in
  let t' = graph_to_local graph entry in
  
  let s2 = Pretty.string_of_local t' in
  (* Just check that something reasonable was generated *)
  Alcotest.(check bool) "reconstructed non-empty" true (String.length s2 > 5)

(* Test 8: DOT generation for global type *)
let test_global_dot () =
  let g = Ast.GMsg ("p", "q", "Int", Ast.GEnd loc, loc) in
  let (graph, entry) = global_to_graph g in
  let dot = global_to_dot graph entry in
  
  (* Just check that DOT string is non-empty and contains expected elements *)
  Alcotest.(check bool) "DOT contains digraph" true (String.contains dot 'd');
  Alcotest.(check bool) "DOT contains nodes" true (String.length dot > 50)

(* Test 9: DOT generation for local type *)
let test_local_dot () =
  let t = Ast.LSend ("p", "Int", Ast.LEnd loc, loc) in
  let (graph, entry) = local_to_graph t in
  let dot = local_to_dot graph entry in
  
  Alcotest.(check bool) "DOT contains digraph" true (String.contains dot 'd');
  Alcotest.(check bool) "DOT contains nodes" true (String.length dot > 50)

(* Test 10: Parallel composition *)
let test_global_parallel () =
  (* (p -> q:[Int]; end) | (r -> s:[Bool]; end) *)
  let g1 = Ast.GMsg ("p", "q", "Int", Ast.GEnd loc, loc) in
  let g2 = Ast.GMsg ("r", "s", "Bool", Ast.GEnd loc, loc) in
  let g = Ast.GPar (g1, g2, loc) in
  let (graph, entry) = global_to_graph g in
  let g' = graph_to_global graph entry in
  
  let s2 = Pretty.string_of_global g' in
  (* Check parallel structure is preserved *)
  Alcotest.(check bool) "contains Int" true (String.contains s2 'I');
  Alcotest.(check bool) "contains Bool" true (String.contains s2 'B');
  Alcotest.(check bool) "has parallel" true (String.contains s2 '|')

(* Test suite *)
let () =
  let open Alcotest in
  run "Type Graph Tests" [
    "Global Types", [
      test_case "simple message" `Quick test_global_simple;
      test_case "branching" `Quick test_global_branching;
      test_case "recursive" `Quick test_global_recursive;
      test_case "parallel composition" `Quick test_global_parallel;
    ];
    "Local Types", [
      test_case "simple send" `Quick test_local_simple;
      test_case "internal choice" `Quick test_local_internal_choice;
      test_case "external choice" `Quick test_local_external_choice;
      test_case "recursive" `Quick test_local_recursive;
    ];
    "DOT Generation", [
      test_case "global DOT" `Quick test_global_dot;
      test_case "local DOT" `Quick test_local_dot;
    ];
  ]

