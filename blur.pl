:- module(blur, 
    [
    blur_mood/2
    blur_sleep/2,
    blur_suicide/2,
    blur_visual_hallucinations/2,
    blur_sound_hallucinations/2,
    blur_addiction/2,
    blur_stress/2,
    blur_differ/2,
    blur_weight_change/2,
    blur_difficulty_concentrating/2,
    blur_panic_attacks/2,
    blur_suspiciousness/2
    ]).

blur_mood(Mood, Factor) :-
	(
		Mood == 0 -> Factor is 0;
		Mood == 0.5 -> Factor is 0.5;
		Mood == 1 -> Factor is 1
	).
blur_sleep(Sleep,Factor) :-
	(
		Sleep == nie -> Factor is 0;
		Sleep == tak -> Factor is 1
	).
blur_suicide(Suicide,Factor) :-
	(
		Suicide == nie -> Factor is 0;
		Suicide == tak -> Factor is 1
	).
blur_visual_hallucinations(Vh,Factor):-
	(
		Vh == brak -> Factor is 0;
		Vh == rzadko -> Factor is 0.5;
		Vh == czesto -> Factor is 1
	).
blur_sound_hallucinations(Sh,Factor):-
	(
		Sh == brak -> Factor is 0;
		Sh == rzadko -> Factor is 0.5;
		Sh == czesto -> Factor is 1
	).
blur_addiction(Addiction,Factor) :-
	(
		Addiction == nie -> Factor is 0;
		Addiction == tak -> Factor is 1
	).
blur_stress(Stress,Factor):-
	(
		Stress == 0 -> Factor is 0;
		Stress == 0.5 -> Factor is 0.5;
		Stress == 1 -> Factor is 1
	).
blur_differ(Differ,Factor):-
	(
		Differ == 0 -> Factor is 0;
		Differ == 0.5 -> Factor is 0.5;
		Differ == 1 -> Factor is 1
	).
blur_weight_change(Weight,Factor):-
	(
		Weight == 0 -> Factor is 0;
		Weight == 0.5 -> Factor is 0.5;
		Weight == 1 -> Factor is 1
	).
blur_difficulty_concentrating(Conc,Factor):-
	(
		Conc == nie -> Factor is 0;
		Conc == tak -> Factor is 1
	).
blur_panic_attacks(Panic,Factor):-
	(
		Panic == nie -> Factor is 0;
		Panic == tak -> Factor is 1
	).
blur_suspiciousness(Sus,Factor):-
	(
		Sus == 0 -> Factor is 0;
		Sus == 0.5 -> Factor is 0.5;
		Sus == 1 -> Factor is 1
	).
