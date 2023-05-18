:- module(misc, 
    [print_list/1, 
    get_mean_value/2, 
    sum_list/2,
	show_results/1
    ]).


print_list([]) :- 
	format('~n').

print_list([Head|Rest]) :-
	(
		Rest == [] -> format('~w',Head);
		format('~w, ',Head)
	),
	print_list(Rest).

get_mean_value(List,Result):-
    sum_list(List,Sum),
	length(List,Length),
	(
		Length == 0 -> Result is 0;
		Result is Sum/Length
	).

sum_list([], 0).
sum_list([ListH|ListT], Sum) :-
    sum_list(ListT, TailSum),
    Sum is ListH + TailSum.

% list_length([],0).
% list_length([_H|T],L+1):-
% 	list_length(T,L).

show_results([]) :- 
	format('~n').

show_results([DiseaseAndProbabilityHead|DiseaseAndProbabilityTail]) :-
	get_key_and_value(DiseaseAndProbabilityHead, Factor, Disease),
	FactorPercentage is Factor * 100,
	format('~2f% ~w ~n', [FactorPercentage, Disease]),
	show_results(DiseaseAndProbabilityTail).

get_key_and_value(KeyValuePair, Key, Value) :-
	pairs_keys_values([KeyValuePair], [Key], [Value]).
