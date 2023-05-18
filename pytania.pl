:- module(pytania, [ask_questions/1]).

use_module(helpers).

ask_questions([M,SL,SU,VH,SH,A,S,D,WC,DC,PA,SUS]) :-
	ask_mood(M),
	ask_sleep(SL),
	ask_suicide(SU),
	ask_visual_hallucinations(VH),
	ask_sound_hallucinations(SH),
    ask_addiction(A),
    ask_stress(S),
    ask_differ(D),
	ask_weight_change(WC),
	ask_difficulty_concentrating(DC),
	ask_panic_attacks(PA),
	ask_suspiciousness(SUS).

% fuzzy
ask_mood(M) :-
	format('Jaki nastrój odczuwa na co dzień pacjent ?~n'),
    fuzzy_list(Frequencies),
	print_list(Frequencies),
	read(M).

ask_sleep(SL) :-
	format('Czy pacjent ma problemy za snem ?~n'),
    yesno_list(Frequencies),
	print_list(Frequencies),
	read(SL).

ask_suicide(SU) :-
	format('Czy pacjent ma myśli samobójcze ?~n'),
    yesno_list(Frequencies),
	print_list(Frequencies),
	read(SU).

ask_visual_hallucinations(VH) :-
	format('Czy pacjent odczuwa halucynacje wzrokowe ?~n'),
    frequency_list(Frequencies),
	print_list(Frequencies),
	read(VH).

ask_sound_hallucinations(SH) :-
	format('Czy pacjent odczuwa halucynacje słuchowe ?~n'),
    frequency_list(Frequencies),
	print_list(Frequencies),
	read(VH).

ask_addiction(A):-
	format('Czy pacjent ma jakieś uzależnienia ?~n'),
    yesno_list(Frequencies),
	print_list(Frequencies),
	read(A).

% fuzzy
ask_stress(S) :-
	format('W jak dużym stopniu pacjent odczuwa stres w życiu codziennym?~n'),
	fuzzy_list(Frequencies),
	print_list(Frequencies),
	read(S).

% fuzzy
ask_differ(D) :-
	format('Jak bardzo pacjent czuje, że nie pasuje do społeczeństw?~n'),
	fuzzy_list(Frequencies),
	print_list(Frequencies),
	read(D).

% fuzzy
ask_weight_change(WC) :-
	format('Jak zmieniła się waga pacjenta w okresie ostatniego rok?~n'),
	fuzzy_list(Frequencies),
	print_list(Frequencies),
	read(WC).

ask_difficulty_concentrating(DC) :-
	format('Czy pacjent odczuwa trudność w skupieniu uwagu ?~n'),
    yesno_list(Frequencies),
	print_list(Frequencies),
	read(DC).

ask_panic_attacks(PA) :-
	format('Czy pacjent odczuwa ataki paniki?~n'),
    yesno_list(Frequencies),
	print_list(Frequencies),
	read(PA).
% fuzzy
ask_suspiciousness(SUS) :-
	format('Jakie zaufanie ma pacjent wobec lekarza?~n'),
	fuzzy_list(Frequencies),
	print_list(Frequencies),
	read(SUS).

frequency_list([brak,rzadko,czesto]).
yesno_list([tak,nie]).
fuzzy_list([0, 0.5, 1]).

:-[helpers].
