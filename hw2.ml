type ('nonterminal, 'terminal) symbol = N of 'nonterminal | T of 'terminal;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* 
	This makes left recursion work, but causes it to fail with empty production 
	rules, ie those of the form A->[empty string] 
*)
let left_recursion = false;;



(* 
	*** Question 1: Convert hw1 grammar to hw2 grammars ***
*)
let rec get_rhs rules lhs = match rules with
	| [] -> []
	| hd::tl ->
			if (lhs == (fst hd)) then
				(snd hd) :: (get_rhs tl lhs)
			else
				(get_rhs tl lhs)
;;

let convert_grammar gram =
	(fst gram), (get_rhs (snd gram))
;;



(* 
	*** Question 2: Parse tree leaves and return leaves  ***
*)
let parse_tree_leaves tree =
	let rec tree_leaves_helper tree_list = match tree_list with
		| [] -> [] 
		| hd :: tl -> match hd with 
			| Node (nt, tree) -> (tree_leaves_helper tree) @ (tree_leaves_helper tl)
			| Leaf l -> l :: (tree_leaves_helper tl)
	in
	tree_leaves_helper [tree]
;;



(* 
	*** Question 3: Checks whether a fragment matches a grammar and returns 
	result of the acceptor ***
*)

(* 
	Iterates through all the options and applies func on it until options 
	exhausted 
*)
let rec option_iterator symbols rules accept frag options func =
	match options with
	| [] -> None (* No more options, fail *)
	| opt_hd::opt_tl ->
		let result = func (opt_hd @ symbols) rules accept frag in
		match result with 
		| None -> (* This option failed, try next one *)
			option_iterator symbols rules accept frag opt_tl func 
		| a -> a (* Return if successful *)

let rec matcher_helper
	(symbols : ('nonterm,'term) symbol list) 
	(rules : ('nonterm -> ('nonterm, 'term) symbol list list)) 
	(accept : ('term list -> 'a option))
	(frag : 'term list)
	: 'a option =

	(* If prefix totally filled out, return result of acceptor *)
	if (symbols = []) then accept frag else

	(* 
		Fast termination condition, every symbol generates >= 1 nonterm so fail.
		Protects against left recursion too 
	*)
	if (left_recursion && ((List.length symbols) > (List.length frag))) then None else

	(* Otherwise inspect the first symbol *)
	match (List.hd symbols) with 
	| T term ->
		(match frag with
		| [] -> None
		| f_hd::f_tl -> 
			if term = f_hd then (* Successfully paired off, parser remaining *)
			matcher_helper (List.tl symbols) rules accept (List.tl frag)
			else (* Fail, try and backtrack at a higher level *)
				None)

	| N nonterm -> 
		let options = rules nonterm in
		if options = [] then
			option_iterator (List.tl symbols) rules accept frag [[]] matcher_helper
		else
			option_iterator (List.tl symbols) rules accept frag options matcher_helper
;;

let make_matcher gram = 
	matcher_helper [N (fst gram)] (snd gram)
;;



(*
	*** Question 4: Generate parse tree for a fragment ***
*)

(* 
	Same as original option iterator, but tacks on successful rules on to the 
	return value 
*)
let rec option_iterator2 
	(symbols : ('nonterm,'term) symbol list) 
	(rules : ('nonterm -> ('nonterm, 'term) symbol list list)) 
	(frag : 'term list)
	(nonterm : 'nonterm) 
	(options : ('nonterm,'term) symbol list list) 
	func =
	match options with
	| [] -> None (* No more options, fail *)
	| opt_hd::opt_tl ->
		let result = func (opt_hd @ symbols) rules frag in
		match result with 
		| None -> (* This option failed, try next one *)
			option_iterator2 symbols rules frag nonterm opt_tl func 
		| Some a -> Some ((nonterm, opt_hd) :: a) (* Return if successful *)

(* 
	Gets the derivation of the fragment given a grammar, rules listed in order
 	that they are used 
*)
let rec get_derivation
	(symbols : ('nonterm,'term) symbol list) 
	(rules : ('nonterm -> ('nonterm, 'term) symbol list list)) 
	(frag : 'term list)
	: ('nonterm * ('nonterm,'term) symbol list) list option =

	(* If fragment and symbols simultaneously exhausted, then success *)
	if symbols = [] && frag = [] then Some [] else

	(* If symbols are exhausted without fragment being exhausted, fail *)
	if symbols = [] then None else

	(* 
		Fast termination condition, every symbol generates >= 1 nonterm so fail.
		Protects against left recursion too.
	*)
	if (left_recursion && ((List.length symbols) > (List.length frag))) then None else

	(* Otherwise inspect the first symbol *)
	match (List.hd symbols) with 
	| T term ->
		(match frag with
		| [] -> None
		| f_hd::f_tl -> 
			if term = f_hd then (* Successfully paired off, parser remaining *)
				get_derivation (List.tl symbols) rules (List.tl frag)
			else (* Fail, try and backtrack at a higher level *)
				None)

	| N nonterm -> 
		let options = rules nonterm in
		if options = [] then (* Edge case for empty options *)
			option_iterator2 (List.tl symbols) rules frag nonterm [[]] get_derivation
		else 
			option_iterator2 (List.tl symbols) rules frag nonterm options get_derivation
;;

(*
	Converts a (pre-order) derivation into a parse tree.
	Returns the subtree generated up to some terminals and the subtree
*)
let rec to_ptree 
	(deriv : ('nonterm * ('nonterm,'term) symbol list) list) 
	: ('nonterm * ('nonterm,'term) symbol list) list * ('nonterm,'term) parse_tree =

	(* 
		Iterates through the symbols on the rhs and builds out the subtrees for them 
	*)
	let rec ptree_iterator 
		(deriv : ('nonterm * ('nonterm,'term) symbol list) list)
		(rhs : ('nonterm,'term) symbol list) 
		: ('nonterm * ('nonterm,'term) symbol list) list * ('nonterm,'term) parse_tree list =
			match rhs with 
			| [] -> deriv, [] (* Note this keeps line 183 safe *)
			| r_hd::r_tl -> 
				match r_hd with
				| T term -> (* Get rest of results then attach leaf for this terminal *)
					let res = ptree_iterator deriv r_tl in
					(match res with | (remaining_deriv, subtrees) ->
						remaining_deriv, ((Leaf	term) :: subtrees))

				| N nonterm -> (* Expand out for this nonterm, then expand rest of rhs *)
					let expansion = to_ptree deriv in
					match expansion with | (remaining_deriv, cur_node) ->

					let res = ptree_iterator remaining_deriv r_tl in
					match res with | (remaining_deriv2, subtrees2) ->
						remaining_deriv2, (cur_node :: subtrees2)
	in

	match deriv with
	(* Will get a warning because of not having a [] case, but logically 
		impossible to hit, see line 163 *)
	| d_hd::d_tl -> match d_hd with (lhs, rhs) ->
		(* Iterate through right hand side and build out subtrees for each symbol *)
		let res = ptree_iterator d_tl rhs in
		match res with (remaining_deriv, subtrees) ->
			(* Make a node with this lhs and attach subtrees of rhs symbols under *)
			remaining_deriv, Node (lhs, subtrees)
;;

(*
	Thin wrapper to get currying to work
*)
let parser_wrapper gram frag =
	let deriv = get_derivation [N (fst gram)] (snd gram) frag in
	match deriv with
	| None -> None
	| Some d -> 
		let res = to_ptree d in
		match res with | (junk, ptree) -> Some ptree
;;

let make_parser gram = 
	parser_wrapper gram
;;