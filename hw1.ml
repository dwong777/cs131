(*
	*** Generic Utilities ***
*)
(* Removes duplicates from a sorted list *)
let rec remove_duplicates lst = 
	match lst with
	| [] -> []
	| x :: [] -> x :: []
	| x :: y :: tl -> 
		if (x = y) then (* Skip x *)
			remove_duplicates (List.tl lst)
		else (* x is concatenated on our return *)
			x :: remove_duplicates (List.tl lst)
;;

(* Sorts and removes duplicates of a list *)
let make_set lst =
	remove_duplicates (List.sort compare lst)
;;

(* Selects elements in a list at the given indices *)
let select_indices lst indices = 
	let rec select_helper lst indices i = 
		match indices with
		| [] -> []
		| hd :: tl -> 
			if (hd = i) then
				(List.hd lst) :: (select_helper (List.tl lst) tl (i+1))
			else
				(select_helper (List.tl lst) indices (i+1))
	in

	select_helper lst indices 0
;;

(* 
	*** Question 1: Determine if a is a subset of b ***
*)
let subset a b =
	let set_a = make_set a in
	let set_b = make_set b in

	let rec subset_helper a b =
		if (a = []) then (* Every element of a has been matched with an element of b *) 
			true 
		else if (b = []) then (* a isn't empty but b is, so not a subset *)
			false
		else if ((List.hd a) = (List.hd b)) then (* Check next a and b *)
			subset_helper (List.tl a) (List.tl b) 
		else (* Try to check next b *)
			subset_helper a (List.tl b)
	in

	subset_helper set_a set_b
;;

(*
	*** Question 2: Determine if set a = set b ***
*)
let equal_sets a b =
	subset a b && subset b a
;;

(* 
	*** Question 3: Set union ***
*)
let set_union a b =
	make_set (a @ b)
;;

(*
	*** Question 4: Set intersection ***
*)
let set_intersection a b =
	let set_a = make_set a in
	let set_b = make_set b in

	let rec intersect_helper a b =
		if (a = [] || b = []) then (* Intersecting with empty is empty *)
			[]
		else
			let hda = List.hd a in
			let hdb = List.hd b in
			if (hda = hdb) then
				hda :: intersect_helper (List.tl a) (List.tl b)
			else if (hda < hdb) then
				intersect_helper (List.tl a) b
			else (* hda > hdb *)
				intersect_helper a (List.tl b)
	in

	intersect_helper set_a set_b
;;

(*
	*** Question 5: Set difference ***
*)
let set_diff a b =
	let set_a = make_set a in 
	let set_b = make_set b in 

	let rec diff_helper a b =
		if (a = []) then
			[]
		else if (b = []) then
			a
		else
			let hda = List.hd a in
			let hdb = List.hd b in

			if (hda = hdb) then
				diff_helper (List.tl a) (List.tl b)
			else if (hda < hdb) then
				hda :: diff_helper (List.tl a) b
			else
				hdb :: diff_helper a (List.tl b)
	in

	diff_helper set_a set_b
;;

(*
	*** Question 6: Computed fixed point ***
*)
let computed_fixed_point eq f x =
	let fx = f x in

	let rec fp_helper eq f x prev =
		if eq x prev then
			prev
		else
			fp_helper eq f (f x) x
	in

	fp_helper eq f fx x
;;

(*
	*** Question 7: Unreachable rules ***
*)
type ('nonterminal, 'terminal) symbol = N of 'nonterminal | T of 'terminal;;

(* 
	Helper function that check which rules have a certain symbol on the left, 
	and returns their indices as a set 
*)
let check_rules rules sym = 
	let rec check_rules_helper rules sym i =
		match rules with
		| [] -> []
		| r :: tl -> 
			match r with (lhs, rhs) ->
			if (lhs = sym) then 
				i :: (check_rules_helper tl sym (i+1))
			else
				(check_rules_helper tl sym (i+1))
	in
	check_rules_helper rules sym 0
;;

let filter_reachable g =

	(* Searches through all rules with sym on lhs of the rule *)
	let rec filter_reachable_helper sym rules_full prev = 

		(* Function that helps us actually iterate through all possible rules *)
		let rec rules_iterator sym rules_full rules prev = 

			(* Iterates through rhs of rule and searches further if nonterminal *)
			let rec rhs_iterator rhs rules_full prev =
				match rhs with
				| [] -> prev (* Exhausted rhs, break *)
				| hd :: tl -> 
					match hd with 
					| T terminal -> (* Terminal symbols can't be expanded *)
						(rhs_iterator tl rules_full prev)
					| N nonterminal -> (* Expand rules with this symbol as lhs *)

						(set_union 
							(* [nonterminal] *)
							(filter_reachable_helper nonterminal rules_full prev)
							(rhs_iterator tl rules_full prev)
						)
			in

			match rules with 
			| [] -> [] (* Rules exhausted, break searching *)
			| r :: tl -> match r with (lhs, rhs) ->
				if (lhs = sym) then (* Symbol matches, search here*)
					(set_union 
						(rhs_iterator rhs rules_full prev)
						(rules_iterator sym rules_full tl prev)
					)
				else
					(rules_iterator sym rules_full tl prev)
		in

		let cur = check_rules rules_full sym in (* Determines indices of rules with sym on lhs *)
		if (subset cur prev) then (* If no new rules encountered, end recursion *)
			prev
		else (* Iterate through all rules with sym on lhs and go through their rhs *)
			(rules_iterator sym rules_full rules_full (set_union cur prev))
	in

	match g with (start, rules) ->
		(
			start,
			(select_indices rules (filter_reachable_helper start rules []))
		)
;;