:- module(app, [main/0]).

:- use_module(user_interface).
:- use_module(variables).

main :-
	format('Diagnosis.~n'),
	format('Answer questions to get mental illness diagnosis.~n'),
	identify_illness.

recommender_menu :-
	format('Diagnosis.~n'),
	format('Answer questions to get mental illness diagnosis.~n'),
	identify_illness.
