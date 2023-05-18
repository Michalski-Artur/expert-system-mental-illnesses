:- module(logistics, 
    [analyze_visual_hallucinations/2,
    analyze_hearing_hallucinations/2,
    analyze_somatic_hallucinations/2,
    analyze_thinking_disorder/2,
    analyze_high_stress/2,
    analyze_communicativeness/2,
    analyze_weight_change/2,
    analyze_difficulty_concentrating/2,
    analyze_panic_attacks/2,
    analyze_suspiciousness/2]).

:- consult(database).
:- use_module(database).

:- use_module(blurring).

analyze_visual_hallucinations(Disease,Factor):-
    database:visual_hallucinations(Disease,Data),
    blur_visual_hallucinations(Data, Factor).

analyze_hearing_hallucinations(Disease,Factor):-
    database:hearing_hallucinations(Disease,Data),
    blur_hearing_hallucinations(Data, Factor).

analyze_somatic_hallucinations(Disease,Factor):-
    database:somatic_hallucinations(Disease,Data),
    blur_somatic_hallucinations(Data, Factor).

analyze_thinking_disorder(Disease,Factor):-
    database:thinking_disorder(Disease,Data),
    blur_thinking_disorder(Data, Factor).

analyze_high_stress(Disease,Factor) :-
    no_stress(Disease, NoF),
    average_stress(Disease, AvgF),
    lots_stress(Disease, LotsF),
    map_fuzzy_to_factor(little, LitteFuz),
    map_fuzzy_to_factor(average, AverageFuz),
    map_fuzzy_to_factor(lots, LotsFuz),
    sharpen(
    [NoF, AvgF, LotsF],
    [LitteFuz, AverageFuz, LotsFuz],
    Factor).

no_stress(Disease,Factor) :-
    database:high_stress(Disease,Stress),
    (
        Stress < 0.2 -> Factor is 1.0;
        Stress >= 0.2, Stress < 0.4 -> down_slope(0.2, 0.4, Stress, Factor);
        Factor is 0
    ).

average_stress(Disease, Factor) :-
    database:high_stress(Disease,Stress),
    (
        Stress >= 0.2, Stress < 0.4 -> up_slope(0.2, 0.4, Stress, Factor);
        Stress >= 0.4, Stress < 0.6 -> Factor is 1;
        Stress >= 0.6, Stress < 0.8 -> down_slope(0.6, 0.8, Stress, Factor);
        Factor is 0
    ).

lots_stress(Disease, Factor) :-
    database:high_stress(Disease,Stress),
    (
        Stress >= 0.6, Stress < 0.8 -> up_slope(0.6, 0.8, Stress, Factor);
        Stress >= 0.8 -> Factor is 1;
        Factor is 0
    ).

analyze_communicativeness(Disease,Factor):-
    no_comm(Disease, NoF),
    average_comm(Disease, AvgF),
    lots_comm(Disease, LotsF),
    map_fuzzy_to_factor(little, LitteFuz),
    map_fuzzy_to_factor(average, AverageFuz),
    map_fuzzy_to_factor(lots, LotsFuz),
    sharpen(
    [NoF, AvgF, LotsF],
    [LitteFuz, AverageFuz, LotsFuz],
    Factor).

no_comm(Disease,Factor) :-
    database:communicativeness(Disease,Comm),
    (
        Comm < 0.2 -> Factor is 1.0;
        Comm >= 0.2, Comm < 0.4 -> down_slope(0.2, 0.4, Comm, Factor);
        Factor is 0
    ).

average_comm(Disease, Factor) :-
    database:communicativeness(Disease,Comm),
    (
        Comm >= 0.2, Comm < 0.4 -> up_slope(0.2, 0.4, Comm, Factor);
        Comm >= 0.4, Comm < 0.6 -> Factor is 1;
        Comm >= 0.6, Comm < 0.8 -> down_slope(0.6, 0.8, Comm, Factor);
        Factor is 0
    ).

lots_comm(Disease, Factor) :-
    database:communicativeness(Disease,Comm),
    (
        Comm >= 0.6, Comm < 0.8 -> up_slope(0.6, 0.8, Comm, Factor);
        Comm >= 0.8 -> Factor is 1;
        Factor is 0
    ).

analyze_weight_change(Disease,Factor):-
    no_weight(Disease, NoF),
    average_weight(Disease, AvgF),
    lots_weight(Disease, LotsF),
    map_fuzzy_to_factor(little, LitteFuz),
    map_fuzzy_to_factor(average, AverageFuz),
    map_fuzzy_to_factor(lots, LotsFuz),
    sharpen(
    [NoF, AvgF, LotsF],
    [LitteFuz, AverageFuz, LotsFuz],
    Factor).

no_weight(Disease,Factor):-
    database:weight_change(Disease,WeightChange),
    (
        WeightChange < 0.2 -> Factor is 1.0;
        WeightChange >= 0.2, WeightChange < 0.4 -> down_slope(0.2, 0.4, WeightChange, Factor);
        Factor is 0
    ).

average_weight(Disease, Factor):-
    database:weight_change(Disease,WeightChange),
    (
        WeightChange >= 0.2, WeightChange < 0.4 -> up_slope(0.2, 0.4, WeightChange, Factor);
        WeightChange >= 0.4, WeightChange < 0.6 -> Factor is 1;
        WeightChange >= 0.6, WeightChange < 0.8 -> down_slope(0.6, 0.8, WeightChange, Factor);
        Factor is 0
    ).

lots_weight(Disease, Factor):-
    database:weight_change(Disease,WeightChange),
    (
        WeightChange >= 0.6, WeightChange < 0.8 -> up_slope(0.6, 0.8, WeightChange, Factor);
        WeightChange >= 0.8 -> Factor is 1;
        Factor is 0
    ).

analyze_difficulty_concentrating(Disease,Factor):-
    database:difficulty_concentrating(Disease,Data),
    blur_difficulty_concentrating(Data, Factor).

analyze_panic_attacks(Disease,Factor):-
    database:panic_attacks(Disease,Data),
    blur_panic_attacks(Data, Factor).

analyze_suspiciousness(Disease,Factor):-
    no_sus(Disease, NoF),
    average_sus(Disease, AvgF),
    lots_sus(Disease, LotsF),
    map_fuzzy_to_factor(little, LitteFuz),
    map_fuzzy_to_factor(average, AverageFuz),
    map_fuzzy_to_factor(lots, LotsFuz),
    sharpen(
    [NoF, AvgF, LotsF],
    [LitteFuz, AverageFuz, LotsFuz],
    Factor).

no_sus(Disease,Factor):-
    database:suspiciousness(Disease,Sus),
    (
        Sus < 0.2 -> Factor is 1.0;
        Sus >= 0.2, Sus < 0.4 -> down_slope(0.2, 0.4, Sus, Factor);
        Factor is 0
    ).

average_sus(Disease, Factor):-
    database:suspiciousness(Disease,Sus),
    (
        Sus >= 0.2, Sus < 0.4 -> up_slope(0.2, 0.4, Sus, Factor);
        Sus >= 0.4, Sus < 0.6 -> Factor is 1;
        Sus >= 0.6, Sus < 0.8 -> down_slope(0.6, 0.8, Sus, Factor);
        Factor is 0
    ).

lots_sus(Disease, Factor):-
    database:suspiciousness(Disease,Sus),
    (
        Sus >= 0.6, Sus < 0.8 -> up_slope(0.6, 0.8, Sus, Factor);
        Sus >= 0.8 -> Factor is 1;
        Factor is 0
    ).

map_fuzzy_to_factor(Fuzzy, Factor):-
    (
        Fuzzy = little -> Factor is 0;
        Fuzzy = average -> Factor is 0.5;
        Fuzzy = lots -> Factor is 1
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
	[database].
