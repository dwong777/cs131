(*
	*** Question 8: Test cases ***
		Note: Some of these are the same as the ones in the sample. The ones I 
		wrote myself are marked by "new"
*)

let my_subset_test0 = subset [] [1;2;3]
let my_subset_test1 = subset [3;1;3] [1;2;3]
let my_subset_test2 = not (subset [1;3;7] [4;1;3])
(* New *)
let my_subset_test3 = not (subset ['a';'b';'d'] ['c';'b';'b';'a'])
let my_subset_test4 = subset ["a"] ["a";"c"]
let my_subset_test5 = not (subset ["a"] [])

let my_equal_sets_test0 = equal_sets [1;3] [3;1;3]
let my_equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])
(* New *)
let my_equal_sets_test2 = equal_sets [] []
let my_equal_sets_test3 = not (equal_sets [] [1;2])
let my_equal_sets_test4 = not (equal_sets [3;4] [])

let my_set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [] []) []
(* New *)
let my_set_union_test3 = not (equal_sets (set_union [1] [2]) [1]) 

let my_set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1]
(* New *)
let my_set_intersection_test3 = 
  equal_sets (set_intersection [] []) []

let my_set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4]
let my_set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let my_set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []
(* New *)
let my_set_diff_test4 = equal_sets (set_diff [] []) []

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let my_computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let my_computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)
(* New *)
let my_computed_fixed_point_test4 =
  computed_fixed_point (=) (fun x -> x *. x) 0.99 = 0.
(* Negative test case will cause program to hang  *)
(* let my_computed_fixed_point_test5 = 
  computed_fixed_point (=) (fun x -> x + 1) 1 = 0  *)

(* Omitting grammars from sample test, clutter screen too much *)

(* New *)
type test_nonterminals =
  | A | B | C | X | Y | Z

let test_rules =
[A, [N A]; (* Should reach only rules with A *)
 A, [T "a"];
 B, [N B; N C]; (* Should reach all *)
 B, [N Y];
 C, [N A; N C]; (* Should reach rules with A and C *)
 C, [N C; T "c"; N A];
 X, [T "x"]; (* Should reach rules with X and Z *)
 X, [N X; N Z];
 Y, [N B]; (* Should reach rules all *)
 Y, [N Y];
 Y, [N Z];
 Z, [N X; T "z"]] (* Should reach rules with X and Z *)

let my_filter_reachable_test0 = 
	(filter_reachable (A, test_rules)) = (A,
		[A, [N A];
		 A, [T "a"]]
	)

let my_filter_reachable_test1 = 
	(filter_reachable (B, test_rules)) = (B, test_rules)

let my_filter_reachable_test2 = 
	(filter_reachable (C, test_rules)) = (C, 
		[A, [N A];
		 A, [T "a"];
		 C, [N A; N C];
		 C, [N C; T "c"; N A]]
	)

let my_filter_reachable_test3 = 
	(filter_reachable (X, test_rules)) = (X,
		 [X, [T "x"];
		 X, [N X; N Z];
		 Z, [N X; T "z"]]
	)

let my_filter_reachable_test4 = 
	(filter_reachable (Y, test_rules)) = (Y, test_rules)

let my_filter_reachable_test5 = 
	(filter_reachable (Z, test_rules)) = (Z,
		 [X, [T "x"];
		 X, [N X; N Z];
		 Z, [N X; T "z"]]
	)