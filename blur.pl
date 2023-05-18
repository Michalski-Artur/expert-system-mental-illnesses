:- module(blur, 
    [
	blur_fuzzy/2,
	blur_yesno/2,
	blur_freq/2
    ]).


blur_fuzzy(Value, Factor) :-
	(
		Factor is Value
	).


blur_yesno(Value, Factor) :-
	(
		Value == tak -> Factor is 1;
		Value == nie -> Factor is 0
	).


blur_freq(Value, Factor) :-
	(
		Value == brak -> Factor is 0;
		Value == rzadko -> Factor is 0.5;
		Value == czesto -> Factor is 1
	).
