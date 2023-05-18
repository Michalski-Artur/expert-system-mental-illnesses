:- module(pytania, [ask_questions/1, fuzzy_list/1]).

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

ask_mood(M) :-
	format('Jaki nastrój odczuwa na co dzień pacjent (0-fatalny, 1 - doskonały)?~n'),
    fuzzy_list(Choices),
	print_list(Choices),
	read(M).

ask_sleep(SL) :-
	format('Czy pacjent ma problemy za snem ?~n'),
    yesno_list(Choices),
	print_list(Choices),
	read(SL).

ask_suicide(SU) :-
	format('Czy pacjent ma myśli samobójcze ?~n'),
    yesno_list(Choices),
	print_list(Choices),
	read(SU).

ask_visual_hallucinations(VH) :-
	format('Czy pacjent odczuwa halucynacje wzrokowe ?~n'),
    frequency_list(Choices),
	print_list(Choices),
	read(VH).

ask_sound_hallucinations(SH) :-
	format('Czy pacjent odczuwa halucynacje słuchowe ?~n'),
    frequency_list(Choices),
	print_list(Choices),
	read(SH).

ask_addiction(A):-
	format('Czy pacjent ma jakieś uzależnienia ?~n'),
    yesno_list(Choices),
	print_list(Choices),
	read(A).

% fuzzy
ask_stress(S) :-
	format('W jak dużym stopniu pacjent odczuwa stres w życiu codziennym (0 - wcale, 1 - cały czas)?~n'),
	fuzzy_list(Choices),
	print_list(Choices),
	read(S).

% fuzzy
ask_differ(D) :-
	format('Jak bardzo pacjent czuje, że nie pasuje do społeczeństwa? (0 - doskonale pasuje, 1 - w ogóle nie pasuje)~n'),
	fuzzy_list(Choices),
	print_list(Choices),
	read(D).

% fuzzy
ask_weight_change(WC) :-
	format('Jak zmieniła się waga pacjenta w okresie ostatniego rok?(0 - mocno w dół, 1 - mocno w górę)~n'),
	fuzzy_list(Choices),
	print_list(Choices),
	read(WC).

ask_difficulty_concentrating(DC) :-
	format('Czy pacjent odczuwa trudność w skupieniu uwagu?~n'),
    yesno_list(Choices),
	print_list(Choices),
	read(DC).

ask_panic_attacks(PA) :-
	format('Czy pacjent odczuwa ataki paniki?~n'),
    yesno_list(Choices),
	print_list(Choices),
	read(PA).
% fuzzy
ask_suspiciousness(SUS) :-
	format('Jak bardzo pacjent jest podejrzliwy względem lekarza?(0 - ma pełne zaufanie, 1 - zupełny brak zaufania)~n'),
	fuzzy_list(Choices),
	print_list(Choices),
	read(SUS).

frequency_list([brak,rzadko,czesto]).
yesno_list([tak,nie]).
fuzzy_list([0, 0.5, 1]).

:-[helpers].
