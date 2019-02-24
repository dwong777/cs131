%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Part A
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tower(N, T, C) :-
	%% Apply initial constraints on towers so that
	%% 1) There are N rows
	length(T, N),
	%% 2) Every row is N long
	%% 3) Every element in a row is in 1..N
	%% 4) Every element in a row is unique
	constrain_tower_rows(N, T),
	%% 5) Every element in a column is unique
	transpose(T, Tr),
	constrain_tower_columns(Tr),

	%% Apply initial constraints on counts
	C = counts(Top, Bottom, Left, Right),
	length(Top, N),
	length(Bottom, N),
	length(Left, N),
	length(Right, N),

	% Impose labelings
	maplist(fd_labeling, T),

	%% Check whether counts are good
	check_counts(T, Tr, C).

%% Transpose code from stackoverflow
%% From here: https://stackoverflow.com/a/4281159
transpose([], []).
transpose([L | Ls], Ts) :-
	transpose_(L, [L | Ls], Ts).

transpose_([], _, []).
transpose_([_ | Es], Lists0, [Fs | Fss]) :-
	maplist(list_first_rest, Lists0, Fs, Lists),
	transpose_(Es, Lists, Fss).

list_first_rest([L | Ls], L, Ls).
%% End stackoverflow code

%% Flip a matrix along middle
flip(M,F) :- maplist(reverse, M, F).

%% Constrains every tower row to be length N 
%% and for every element to be uniquely in 1..N
constrain_tower_rows(_, []).
constrain_tower_rows(N, [H|T]) :-
	length(H, N),
	fd_domain(H, 1, N),
	fd_all_different(H),
	constrain_tower_rows(N, T).

%% Constrains every column (transposed column row) to have unique elements
constrain_tower_columns([]).
constrain_tower_columns([H|T]) :-
	fd_all_different(H),
	constrain_tower_columns(T).

%% Checks counts satisfy tower on each side
check_counts(T, Tr, counts(Top, Bottom, Left, Right)) :-
	% Top
	count_rows(Tr, Top),
	% Left
	count_rows(T, Left),
	% Bottom
	flip(Tr, FTr),
	count_rows(FTr, Bottom),
	% Right
	flip(T, F),
	count_rows(F, Right).

%% Checks that every row matches tower count
count_rows([],[]).
count_rows([R_head|R_tail], [C_head|C_tail]) :-
	count_row(R_head, C_head),
	count_rows(R_tail, C_tail).

%% Counts row with accumulator trick
count_row([], 0).
count_row([R_head|R_tail], C) :-
	count_row_helper(R_tail, 1, R_head, C).

count_row_helper([], Acc, _, C) :- C = Acc.
count_row_helper([R_head|R_tail], Acc, M, C) :-
	(R_head < M ->
			count_row_helper(R_tail, Acc, M, C);
			AccPlusOne is Acc+1,
			count_row_helper(R_tail, AccPlusOne, R_head, C)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Part B
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plain_tower(N, T, C) :-
	%% Enforce T is N rows
	length(T,N),

	%% Generate list 1..N
	gen_Nlist(N, Orig),
	%% Calculate sum and prod to use as hash values
	row_sum(Orig, Orig_Sum),
	row_prod(Orig, Orig_Prod),
	%% Generate permutations of 1..N
	gen_permutations(Orig, Perms),

	%% Enumerate over permutations for each row
	enumerate_tower_rows(T, Perms),
	%% Enforce that every column is a permutation
	transpose(T, Tr),
	check_tower_columns(Orig, Orig_Sum, Orig_Prod, Tr),

	%% Apply initial constraints on counts
	C = counts(Top, Bottom, Left, Right),
	length(Top, N),
	length(Bottom, N),
	length(Left, N),
	length(Right, N),

	%% Check whether counts are good
	check_counts(T, Tr, C).

%% Generates a list containing all numbers 1..N once each
gen_Nlist(0, R) :- R = [] ,!.
gen_Nlist(N, R) :- 
	Nminus is N-1,
	gen_Nlist(Nminus, Acc),
	R = [N | Acc]. %% Cut to prevent stack overflow on fail

%% Sum up a row with accumulator trick
row_sum(R, Sum) :-
	row_sum(R, 0, Sum), !. %% Cut because one answer
row_sum([], Acc, Sum) :- Sum = Acc.
row_sum([H|T], Acc, Sum) :-
	NewAcc is H + Acc,
	row_sum(T, NewAcc, Sum).

%% Multiply up a row with accumulator trick
row_prod(R, Prod) :-
	row_prod(R, 1, Prod), !. %% Cut because one answer
row_prod([], Acc, Prod) :- Prod = Acc.
row_prod([H|T], Acc, Prod) :-
	NewAcc is H * Acc,
	row_prod(T, NewAcc, Prod).

%% Enumerates over towers where each row is a permutation
enumerate_tower_rows([], _).
enumerate_tower_rows([T_Head|T_Tail], Perms) :-
	member(T_Head, Perms),
	enumerate_tower_rows(T_Tail, Perms).

%% Check that each column (transposed tower row) is a permutation
check_tower_columns(_, _, _, []).
check_tower_columns(Orig, Orig_Sum, Orig_Prod, [Tr_Head|Tr_Tail]) :-
	check_permutation(Orig_Sum, Orig_Prod, Tr_Head),
	check_tower_columns(Orig, Orig_Sum, Orig_Prod, Tr_Tail).
	
%% Quickly checks that a column is a permutation
%% by using its sum and product as hashes
%% Stack will overflow from generating permutations
%% before it's possible to create an N long list 
%% where sum and prod match but isn't a permutation
check_permutation(Orig_Sum, Orig_Prod, Col) :- 
	row_sum(Col, Col_Sum),
	Orig_Sum == Col_Sum,
	row_prod(Col, Col_Prod),
	Orig_Prod == Col_Prod.

%% Generates the permutations for a list
gen_permutations([], R) :- R = [].
gen_permutations([El_head|El_tail], R) :-
	permute(El_tail, [[El_head]], R), !. %% Cut because one answer

permute(_, [], R) :- R = [].
permute([], Perms, R) :- R = Perms.
permute([El_head|El_tail], [Perms_head|Perms_tail], R) :-
	permute_helper(El_head, [], Perms_head, Some_Perms),
	permute([El_head], Perms_tail, Res),
	append(Some_Perms, Res, New_Perms),
	permute(El_tail, New_Perms, R).

%% Generates all possible insertions of El into a list
%% Not really true insertions, because some elements flipped
permute_helper(El, Pre, [], R) :- 
	append(Pre, [El], Joined),
	R = [Joined].

permute_helper(El, Pre, [Post_Head|Post_Tail], R) :-
	append(Pre, [El,Post_Head|Post_Tail], Joined),
	Next_Pre = [Post_Head|Pre],
	permute_helper(El, Next_Pre, Post_Tail, Res),
	R = [Joined|Res].

speedup(S) :-
	statistics(cpu_time, Before_Tower),
	findall(1, tower(4,T,C), _),
	statistics(cpu_time, After_Tower),
	findall(1, plain_tower(4,T,C), _),
	statistics(cpu_time, After_Plain_Tower),

	Before_Tower = [S1, _],
	After_Tower = [S2, _],
	After_Plain_Tower = [S3, _],

	RT1 is S2-S1,
	RT2 is S3-S2,
	S is RT2/RT1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Part C
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \== T2, !.	
