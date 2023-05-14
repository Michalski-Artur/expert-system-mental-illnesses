:- module(db, [illness_is/2]).

:- use_module(engine_interface).

illness_is(schizophrenia, Factor) :-
	make_sure_fuzzy(mood, low, X),
	make_sure(sleep_problems, yes),
	make_sure(suicidal_thoughts, yes),
	make_sure(visual_hallucinations, no),
	make_sure(auditory_hallucinations, no),
	make_sure(addictions, no),
	make_sure_fuzzy(stress_intensity, high, Y),
	make_sure_fuzzy(sense_of_difference, high, Z),
	make_sure_fuzzy(change_in_bodyweight, medium, W),
	make_sure(difficulty_focusing_attention, yes),
	make_sure(panic_attacks, yes),
	make_sure_fuzzy(lack_of_trust, high, V),
	min_list([X, Y, Z, W, V], Factor).
