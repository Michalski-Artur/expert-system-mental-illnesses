:- module(user_interface, [identify_illness/0]).

:- use_module(engine_interface).
:- use_module(variables).
:- use_module(db_rules).

:- dynamic callstack/1.

find_illness(X, Factor) :-
	illness_is(X),
	get_call_stack(Callstack),
	length(Callstack, CL),
	(
		CL > 0 -> min_list(Callstack, Factor);
		Factor is 1
	),
	clear_callstack.

identify_illness :-
	findall((X, Y), find_illness(X, Y), All_illneses),
	max_list_pairs(All_illneses, (T, _)),
	(illness_text(T, Text), writeln(Text) ; writeln(T)),
	clear_facts.

identify_illness :-
	write("Sorry, unable to determine the illness."), nl,
	clear_facts.

max_list_pairs([], R, R).
max_list_pairs([(X, Y)|Xs], (_, Wy), R) :-
	Y > Wy,
	max_list_pairs(Xs, (X, Y), R).
max_list_pairs([(_, Y)|Xs], (Wx, Wy), R) :-
	Y =< Wy,
	max_list_pairs(Xs, (Wx, Wy), R).
max_list_pairs([(X, Y)|Xs], R) :-
	max_list_pairs(Xs, (X, Y), R).
max_list_pairs([], _) :-
	fail.
