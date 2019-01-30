(* 
	*** Part 1 ***
*)
type test_nonterminals =
  | A | B | C | X | Y | Z

(* This grammar has both left and right recursion going on *)
let test_rules =
[
 A, [N A; N A];
 A, [T "a"];
 B, [N B; N C];
 B, [N Y];
 X, [T "x"]; 
 X, [N X; N X];
 X, [N X; N Z];
 Y, [N B; N Y];
 C, [N A; N C]; 
 C, [N C; T "c"; N A];
 C, [N A];
 (* Y, [N Y]; This rule beats program *)
 Y, [N Z];
 Z, [N X; T "z"]];; 

(* 
	*** Part 2 ***
*)
let test_tree =
	Node (5, 
		[Node (6, 
			[Leaf 7;
			 Leaf 8]
		);
		 Node (9,
		 	[Leaf 10; 
		 	Node (11, [Leaf 12])]
		 )
		]
	);;

(*
	*** Part 3 and 4 ***
*)

(* Grammar with only right recursion and empty strings *)
type simple_nonterminals = 
  I | J | K | L

let simple_rules = 
  function
  | I ->
    [[N J; N I];
     [N K; N I]; (* This rule should never be used *)
     [T "i"]]
  | J ->
    [[N K; N L];
    [T "j"]]
  | K ->
    [[T "k"]]
  | L -> []


(*
	*** Bonus: Test against left-recursive grammars ***
*)

let accept_all string = Some string;;
let accept_empty_suffix = function
  | _::_ -> None
  | x -> Some x
;;

let rec accept_xs str = match str with
  | [] -> Some []
  | hd::tl ->
    if (hd = "x") && ((accept_xs tl) != None) then
      Some str
    else 
      None
;;

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let small_awk_frag = ["$"; "1"; "++"; "-"; "2"]

let left_awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Expr; N Binop; N Term];
          [N Term]]
     | Term ->
   [[N Num];
    [N Lvalue];
    [N Incrop; N Lvalue];
    [N Lvalue; N Incrop];
    [T"("; N Expr; T")"]]
     | Lvalue ->
   [[T"$"; N Expr]]
     | Incrop ->
   [[T"++"];
    [T"--"]]
     | Binop ->
   [[T"+"];
    [T"-"]]
     | Num ->
   [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
    [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]);;

(*** Part 1 ***)
let converted_grammar = convert_grammar (Y, test_rules);;
let my_convert_grammar_test0 = (((snd converted_grammar) A) = [[N A; N A]; [T "a"]]);;
let my_convert_grammar_test1 = (((snd converted_grammar) B) = [[N B; N C]; [N Y]]);;
let my_convert_grammar_test2 = (((snd converted_grammar) Z) = [[N X; T "z"]]);;

(*** Part 2 ***)
let my_parse_tree_leaves_test0 = (parse_tree_leaves test_tree) = [7;8;10;12];;
let my_parse_tree_leaves_test1 = (parse_tree_leaves (Node (5, []))) = [];;

(*** Part 3 ***)
let my_make_matcher_test0 = ((make_matcher (I, simple_rules) accept_empty_suffix ["j";"j";"k";"k";"i"]) = Some []);;
let my_make_matcher_test1 = ((make_matcher (I, simple_rules) accept_all ["j";"j";"k";"k";"i"; "k";"j"]) = Some ["k";"j"]);;

let my_make_matcher_test2 = ((make_matcher (I, simple_rules) accept_empty_suffix ["i"; "k"] = None));;
let my_make_matcher_test3 = ((make_matcher (I, simple_rules) accept_all ["i";"k"] = Some ["k"]));;

let my_make_matcher_test4 = ((make_matcher (J, simple_rules) accept_empty_suffix ["k"] = Some []));;

let make_matcher_test = my_make_matcher_test0 && my_make_matcher_test1 && 
  my_make_matcher_test2 && my_make_matcher_test3 && my_make_matcher_test4;;

(*** Part 4 ***)
let my_make_parser_test0 = ((make_parser (I, simple_rules) ["j";"j";"k";"k";"i"]) =
  Some (Node (I,
     [Node (J, [Leaf "j"]);
      Node (I,
        [Node (J, [Leaf "j"]);
         Node (I,
          [Node (J, 
            [Node (K, [Leaf "k"]); 
             Node (L, [])]);
           Node (I,
            [Node (J, 
              [Node (K, [Leaf "k"]); 
               Node (L, [])]);
             Node (I, [Leaf "i"])])])])]))
);;

let my_parse_tree_leaves_test2 = 
  match make_parser (I, simple_rules) ["j";"j";"k";"k";"i"] with
  | None -> false
  | Some tree -> parse_tree_leaves tree = ["j";"j";"k";"k";"i"];; 

let my_make_parser_test1 = ((make_parser (I, simple_rules) ["i"; "k"]) = None);;

let my_make_parser_test2 = ((make_parser (L, simple_rules) []) = 
  Some (Node (L, [])));;

let make_parser_test = my_make_parser_test0 && my_make_parser_test1 && my_make_parser_test2;;

(*** Left recursion ***)

 
(* let left_make_matcher_test0 = ((make_matcher converted_grammar accept_xs ["x";"x";"x";"z"; "x"; "x"]) = Some ["x"; "x"]);;
let left_make_matcher_test1 = ((make_matcher converted_grammar accept_all ["x";"x";"x";"z";"a"]) = Some ["a"]);;
let left_make_matcher_test2 = ((make_matcher converted_grammar accept_xs ["x";"x";"x";"z";"a"]) = None);;
let left_make_matcher_test = my_make_matcher_test0 && my_make_matcher_test1 && my_make_matcher_test2;;

let left_make_parser_test0 = ((make_parser converted_grammar ["x";"x";"x";"z";"a"]) = None);;
let left_make_parser_test1 = 
	((make_parser converted_grammar ["x";"x";"x";"z"]) =
	  Some (Node (Y,
     [Node (Z,
       [Node (X,
         [Node (X, 
         		[Leaf "x"]);
          Node (X, 
        		[Node (X, 
        			[Leaf "x"]); 
        		Node (X, 
        			[Leaf "x"])])]);
        Leaf "z"])]))
	);;

let left_make_matcher_test3 = 
	(make_matcher left_awkish_grammar accept_empty_suffix (small_awk_frag) = (Some []));;

let left_make_matcher_test4 = 
	(make_matcher left_awkish_grammar accept_empty_suffix (small_awk_frag @ ["3"])) = None;;

let left_make_parser_test2 =
  ((make_parser left_awkish_grammar small_awk_frag)
   = Some (Node (Expr,
   [Node (Expr,
     [Node (Term,
       [Node (Lvalue,
         [Leaf "$"; 
         Node (Expr, 
         	[Node (Term, 
         		[Node (Num, 
         			[Leaf "1"])])])]);
       Node (Incrop, 
       	[Leaf "++"])])]);
   Node (Binop, [Leaf "-"]); 
   Node (Term, 
   	[Node (Num, [Leaf "2"])])])));;

let left_make_parser_test3 = 
	((make_parser left_awkish_grammar (small_awk_frag @ ["3"])) = None);;


let left_parse_tree_leaves_test0 = 
	match make_parser left_awkish_grammar small_awk_frag with
	| None -> false
	| Some tree -> parse_tree_leaves tree = small_awk_frag;; 

 *)