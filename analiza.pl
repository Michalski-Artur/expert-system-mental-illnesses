:- module(analiza, 
    [analyze_mood/2,
    analyze_sleep_problems/2,
    analyze_suicidal_thoughts/2,
    analyze_visual_hallucinations/2,
    analyze_auditory_hallucinations/2,
    analyze_addictions/2,
    analyze_stress_intensity/2,
    analyze_difference/2,
    analyze_change_in_bodyweight/2,
    analyze_difficulty_focusing/2,
    analyze_panic_attacks/2,
    analyze_lack_of_trust/2,
    blur_fuzzy/2,
    blur_yesno/2,
    blur_freq/2]).

:- consult(db).
:- use_module(db).



analyze_mood(Disease, Factor) :-
    bad_mood(Disease, NoF),
    average_mood(Disease, AvgF),
    good_mood(Disease, LotsF),
    sharpen(
    [NoF, AvgF, LotsF],
    [0, 0.5, 1],
    Factor).

    


bad_mood(Disease,Factor) :-
    db:mood(Disease, Mood),
    (
        Mood < 0.2 -> Factor is 1.0;
        Mood >= 0.2, Mood < 0.4 -> down_slope(0.2, 0.4, Mood, Factor);
        Factor is 0
    ).

average_mood(Disease, Factor) :-
    db:mood(Disease, Mood),
    (
        Mood >= 0.2, Mood < 0.4 -> up_slope(0.2, 0.4, Mood, Factor);
        Mood >= 0.4, Mood < 0.6 -> Factor is 1;
        Mood >= 0.6, Mood < 0.8 -> down_slope(0.6, 0.8, Mood, Factor);
        Factor is 0
    ).
good_mood(Disease, Factor) :-
    db:mood(Disease, Mood),
    (
        Mood >= 0.6, Mood < 0.8 -> up_slope(0.6, 0.8, Mood, Factor);
        Mood >= 0.8 -> Factor is 1;
        Factor is 0
    ).
    

analyze_sleep_problems(Disease, Factor) :-
    db:sleep_problems(Disease, Data),
    blur_yesno(Data, Factor).

analyze_suicidal_thoughts(Disease, Factor) :-
    db:suicidal_thoughts(Disease, Data),
    blur_yesno(Data, Factor).

analyze_visual_hallucinations(Disease,Factor):-
    db:visual_hallucinations(Disease,Data),
    blur_freq(Data, Factor).

analyze_auditory_hallucinations(Disease,Factor):-
    db:auditory_hallucinations(Disease,Data),
    blur_freq(Data, Factor).

analyze_addictions(Disease,Factor):-
    db:addictions(Disease,Data),
    blur_yesno(Data, Factor).

analyze_stress_intensity(Disease, Factor) :-
    no_stress(Disease, NoF),
    average_stress(Disease, AvgF),
    lots_stress(Disease, LotsF),
    sharpen(
    [NoF, AvgF, LotsF],
    [0, 0.5, 1],
    Factor).

no_stress(Disease,Factor) :-
    db:stress_intensity(Disease, Stress),
    (
        Stress < 0.2 -> Factor is 1.0;
        Stress >= 0.2, Stress < 0.4 -> down_slope(0.2, 0.4, Stress, Factor);
        Factor is 0
    ).

average_stress(Disease, Factor) :-
    db:stress_intensity(Disease, Stress),
    (
        Stress >= 0.2, Stress < 0.4 -> up_slope(0.2, 0.4, Stress, Factor);
        Stress >= 0.4, Stress < 0.6 -> Factor is 1;
        Stress >= 0.6, Stress < 0.8 -> down_slope(0.6, 0.8, Stress, Factor);
        Factor is 0
    ).

lots_stress(Disease, Factor) :-
    db:stress_intensity(Disease, Stress),
    (
        Stress >= 0.6, Stress < 0.8 -> up_slope(0.6, 0.8, Stress, Factor);
        Stress >= 0.8 -> Factor is 1;
        Factor is 0
    ).

analyze_difference(Disease, Factor):-
    no_diff(Disease, NoF),
    average_diff(Disease, AvgF),
    lots_diff(Disease, LotsF),
    sharpen(
    [NoF, AvgF, LotsF],
    [0, 0.5, 1],
    Factor).

no_diff(Disease,Factor) :-
    db:sense_of_difference(Disease,Diff),
    (
        Diff < 0.2 -> Factor is 1.0;
        Diff >= 0.2, Diff < 0.4 -> down_slope(0.2, 0.4, Diff, Factor);
        Factor is 0
    ).

average_diff(Disease, Factor) :-
    db:sense_of_difference(Disease,Diff),
    (
        Diff >= 0.2, Diff < 0.4 -> up_slope(0.2, 0.4, Diff, Factor);
        Diff >= 0.4, Diff < 0.6 -> Factor is 1;
        Diff >= 0.6, Diff < 0.8 -> down_slope(0.6, 0.8, Diff, Factor);
        Factor is 0
    ).

lots_diff(Disease, Factor) :-
    db:sense_of_difference(Disease,Diff),
    (
        Diff >= 0.6, Diff < 0.8 -> up_slope(0.6, 0.8, Diff, Factor);
        Diff >= 0.8 -> Factor is 1;
        Factor is 0
    ).

analyze_change_in_bodyweight(Disease, Factor):-
    no_weight(Disease, NoF),
    average_weight(Disease, AvgF),
    lots_weight(Disease, LotsF),
    sharpen(
    [NoF, AvgF, LotsF],
    [0, 0.5, 1],
    Factor).

no_weight(Disease,Factor):-
    db:change_in_bodyweight(Disease,WeightChange),
    (
        WeightChange < 0.2 -> Factor is 1.0;
        WeightChange >= 0.2, WeightChange < 0.4 -> down_slope(0.2, 0.4, WeightChange, Factor);
        Factor is 0
    ).

average_weight(Disease, Factor):-
    db:change_in_bodyweight(Disease,WeightChange),
    (
        WeightChange >= 0.2, WeightChange < 0.4 -> up_slope(0.2, 0.4, WeightChange, Factor);
        WeightChange >= 0.4, WeightChange < 0.6 -> Factor is 1;
        WeightChange >= 0.6, WeightChange < 0.8 -> down_slope(0.6, 0.8, WeightChange, Factor);
        Factor is 0
    ).

lots_weight(Disease, Factor):-
    db:change_in_bodyweight(Disease,WeightChange),
    (
        WeightChange >= 0.6, WeightChange < 0.8 -> up_slope(0.6, 0.8, WeightChange, Factor);
        WeightChange >= 0.8 -> Factor is 1;
        Factor is 0
    ).

analyze_difficulty_focusing(Disease, Factor):-
    db:difficulty_focusing_attention(Disease,Data),
    blur_yesno(Data, Factor).

analyze_panic_attacks(Disease, Factor):-
    db:panic_attacks(Disease, Data),
    blur_yesno(Data, Factor).

analyze_lack_of_trust(Disease, Factor):-
    no_lack_trust(Disease, NoF),
    average_lack_trust(Disease, AvgF),
    lack_trust(Disease, LotsF),
    sharpen(
    [NoF, AvgF, LotsF],
    [0, 0.5, 1.0],
    Factor).

no_lack_trust(Disease,Factor):-
    db:lack_of_trust(Disease,Sus),
    (
        Sus < 0.2 -> Factor is 1.0;
        Sus >= 0.2, Sus < 0.4 -> down_slope(0.2, 0.4, Sus, Factor);
        Factor is 0
    ).

average_lack_trust(Disease, Factor):-
    db:lack_of_trust(Disease,Sus),
    (
        Sus >= 0.2, Sus < 0.4 -> up_slope(0.2, 0.4, Sus, Factor);
        Sus >= 0.4, Sus < 0.6 -> Factor is 1;
        Sus >= 0.6, Sus < 0.8 -> down_slope(0.6, 0.8, Sus, Factor);
        Factor is 0
    ).

lack_trust(Disease, Factor):-
    db:lack_of_trust(Disease,Sus),
    (
        Sus >= 0.6, Sus < 0.8 -> up_slope(0.6, 0.8, Sus, Factor);
        Sus >= 0.8 -> Factor is 1;
        Factor is 0
    ).


up_slope(X1, X2, Y, Factor):-
    Factor is (Y - X1) / (X2 - X1).

down_slope(X1, X2, Y, Factor):-
    Factor is (X2 - Y) / (X2 - X1).

sharpen(Factors, Values, Result) :-
    sum_products(Factors, Values, Sum),
    sum_list(Factors, Denom),
    (
        Denom == 0 -> Result is 0;
        Result is Sum / Denom
    ).

sum_products([], [], 0).
sum_products([H|T], [H2|T2], Sum) :-
    sum_products(T, T2, Sum2),
    Sum is H * H2 + Sum2.

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Sum2),
    Sum is H + Sum2.

:-
	[db].

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

