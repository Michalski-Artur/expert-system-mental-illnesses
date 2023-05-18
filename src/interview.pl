:- module(interview, [ask_questions/1]).

use_module(misc).

ask_questions([MM,SP,VH,AH,A,SI,SD,CBW,DFA,PA,LT]) :-
    %TODO


ask_mood(M) :-
    format('How would you rate your mood today?~n'),
    mood_list(Choices),
    print_list(Choices),
    read(M).

ask_sleep_problems(SP) :-
    format('Do you experience sleep problems?~n'),
    yesno_list(Choices),
    print_list(Choices),
    read(SP).

ask_suicidal_thoughts(ST) :-
    format('Do you experience suicidal thoughts?~n'),
    yesno_list(Choices),
    print_list(Choices),
    read(ST).

ask_visual_hallucinations(VH) :-
	format('How often do you experience visual hallucinations ?~n'),
    frequency_list(Choices),
	print_list(Choices),
	read(VH).

ask_auditory_hallucinations(AH) :-
	format('How often do you experience hearing hallucinations ?~n'),
    frequency_list(Choices),
	print_list(Choices),
	read(AH).

ask_addictions(A) :-
	format('Do you have any addictions?~n'),
    yesno_list(Choices),
	print_list(Choices),
	read(A).

ask_high_stress(SI) :-
	format('How would you describe your stress level?~n'),
	fuzzy_list(Choices),
	print_list(Choices),
	read(SI).


ask_sense_of_difference(SD) :-
    format('How would you describe your sense of difference?~n'),
    fuzzy_list(Choices),
    print_list(Choices),
    read(SD).

ask_change_in_bodyweight(CBW) :-
	format('How much has your body weight changed in last months?~n'),
	weight_change_list(Choices),
	print_list(Choices),
	read(CBW).

ask_difficulty_focusing_attention(DFA) :-
    format('Do you experience problems with focusing attention?~n'),
    yesno_list(Choices),
    print_list(Choices),
    read(DFA).
    

ask_panic_attacks(PA) :-
	format('Do you experience panic attacs?~n'),
    yesno_list(Choices),
	print_list(Choices),
	read(PA).


ask_lack_of_trust(LT) :-
	format('How would you describe your suspiciousness towards doctor?~n'),
	fuzzy_list(Choices),
	print_list(Choices),
	read(LT).

frequency_list([none,rarely,often]).
yesno_list([yes,no]).
fuzzy_list([little, average, lots]).
mood_list([terrible, bad, average, good, excellent]).
weight_change_list([way_down, sligthly_down, same, slightly_up, way_up]).


:-
	[misc].
