:- module(engine_interface, [make_sure/2, clear_facts/0, clear_callstack/0, get_call_stack/1, debug/0]).

:- use_module(variables).

:- dynamic db_make_sure/2.
:- dynamic db_make_sure/3.
:- dynamic callstack/1.

check_if(X, Y) :-
    question_text(X, Text),
    available_options(X, Options),
    write(Text),
	format("Available options are: "),
    print_variables(Options),
	read(R),
	remember(X,R),
    Y == R.

ask(X, Y) :-
    question_text(X, Text),
    write(Text),
	read(Y).

make_sure(mood, Y) :-
    db_make_sure(mood, Y, Factor), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(mood, Y) :-
    ask(mood, V),
    V is V/10,
    fuzzify_low(V, L),
    fuzzify_medium(V, M),
    fuzzify_high(V, H),
    asserta(db_make_sure(mood, low, L)),
    asserta(db_make_sure(mood, medium, M)),
    asserta(db_make_sure(mood, high, H)),
    (
        Y == low -> Factor is L;
        Y == medium -> Factor is M;
        Y == high -> Factor is H
    ), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(stress_intensity, Y) :-
    db_make_sure(stress_intensity, Y, Factor), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(stress_intensity, Y) :-
    ask(stress_intensity, V),
    V is V/10,
    fuzzify_low(V, L),
    fuzzify_medium(V, M),
    fuzzify_high(V, H),
    asserta(db_make_sure(stress_intensity, low, L)),
    asserta(db_make_sure(stress_intensity, medium, M)),
    asserta(db_make_sure(stress_intensity, high, H)),
    (
        Y == low -> Factor is L;
        Y == medium -> Factor is M;
        Y == high -> Factor is H
    ), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(sense_of_difference, Y) :-
    db_make_sure(sense_of_difference, Y, Factor), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(sense_of_difference, Y) :-
    ask(sense_of_difference, V),
    V is V/10,
    fuzzify_low(V, L),
    fuzzify_medium(V, M),
    fuzzify_high(V, H),
    asserta(db_make_sure(sense_of_difference, low, L)),
    asserta(db_make_sure(sense_of_difference, medium, M)),
    asserta(db_make_sure(sense_of_difference, high, H)),
    (
        Y == low -> Factor is L;
        Y == medium -> Factor is M;
        Y == high -> Factor is H
    ), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(change_in_bodyweight, Y) :-
    db_make_sure(change_in_bodyweight, Y, Factor), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(change_in_bodyweight, Y) :-
    ask(current_weight, CW),
    ask(previous_weight, PW),
    calculate_weight_change(CW, PW, WC),
    fuzzify_low(WC, L),
    fuzzify_medium(WC, M),
    fuzzify_high(WC, H),
    asserta(db_make_sure(change_in_bodyweight, low, L)),
    asserta(db_make_sure(change_in_bodyweight, medium, M)),
    asserta(db_make_sure(change_in_bodyweight, high, H)),
    (
        Y == low -> Factor is L;
        Y == medium -> Factor is M;
        Y == high -> Factor is H
    ), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(lack_of_trust, Y) :-
    db_make_sure(lack_of_trust, Y, Factor), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(lack_of_trust, Y) :-
    ask(lack_of_trust, V),
    V is 10 - V,
    fuzzify_low(V, L),
    fuzzify_medium(V, M),
    fuzzify_high(V, H),
    asserta(db_make_sure(lack_of_trust, low, L)),
    asserta(db_make_sure(lack_of_trust, medium, M)),
    asserta(db_make_sure(lack_of_trust, high, H)),
    (
        Y == low -> Factor is L;
        Y == medium -> Factor is M;
        Y == high -> Factor is H
    ), !,
    Factor > 0,
    asserta(callstack(Factor)).

make_sure(X, Y) :- db_make_sure(X, Z), !, Y == Z.
make_sure(X, Y) :- check_if(X, Y).

remember(X, Y) :- asserta(db_make_sure(X, Y)).

clear_facts :-
    retractall(db_make_sure(_, _)),
    retractall(db_make_sure(_, _, _)).

clear_callstack :-
    retractall(callstack(_)).

get_call_stack(X) :-
    findall(Y, callstack(Y), X).

debug :-
    format("----------------\n"),
    forall(callstack(K), format("callstack(~w)\n", [K])),
    forall(db_make_sure(X, Y), format("db_make_sure(~w, ~w)\n", [X, Y])),
    forall(db_make_sure(A, B, C), format("db_make_sure(~w, ~w, ~w)\n", [A, B, C])),
    format("----------------\n").

calculate_weight_change(CW, PW, WC) :-
    WC is (CW - PW)/PW + 0.5.

fuzzify_low(P, Factor) :-
    (
        P < 0.25 -> Factor is 1;
        P >= 0.25, P < 0.5 -> slope_down(0.25, 0.5, P, Factor);
        Factor is 0
    ).

fuzzify_medium(P, Factor) :-
    (
        P < 0.25 -> Factor is 0;
        P >= 0.25, P < 0.4 -> slope_up(0.25, 0.4, P, Factor);
        P >= 0.4, P < 0.6 -> Factor is 1;
        P >= 0.6, P < 0.75 -> slope_down(0.6, 0.75, P, Factor);
        Factor is 0
    ).

fuzzify_high(P, Factor) :-
    (
        P < 0.5 -> Factor is 0;
        P >= 0.5, P < 0.75 -> slope_up(0.5, 0.75, P, Factor);
        Factor is 1
    ).

slope_down(Left, Right, Value, Factor) :-
    Factor is (Right - Value) / (Right - Left).

slope_up(Left, Right, Value, Factor) :-
    Factor is (Value - Left) / (Right - Left).
