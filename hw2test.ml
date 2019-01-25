type test_nonterminals =
  | A | B | C | X | Y | Z

let test_rules =
[A, [N A]; (* Should reach only rules with A *)
 A, [T "a"];
 B, [N B; N C]; (* Should reach all *)
 B, [N Y];
 X, [T "x"]; (* Should reach rules with X and Z *)
 X, [N X; N Z];
 Y, [N B]; (* Should reach rules all *)
 C, [N A; N C]; (* Should reach rules with A and C *)
 C, [N C; T "c"; N A];
 Y, [N Y];
 Y, [N Z];
 Z, [N X; T "z"]];; (* Should reach rules with X and Z *)

let res = (snd (convert_grammar (A, test_rules))) B;;

let tree_thing =
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

parse_tree_leaves tree_thing;;
