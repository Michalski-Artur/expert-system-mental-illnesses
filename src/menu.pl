:- module(menu, [menu/0]).

use_module(interview).
use_module(diagnosis).
use_module(helpers).
use_module(expert).

menu:-
    write("*************************************"),nl,
    write("Politechnika Warszawska MiNI"),nl,
    write("Projekt z przedmiotu Systemy Ekspertowe, rozpoznawanie chorób psychicznych."),nl,
    repeat,
    write("*************************************"),nl,
    write("* 1. Przeanalizuj przypadek pacjenta."),nl,
    write("* 2. Uruchom modul eksperta."),nl,
    write("* 3. Zamknij aplikacje."),nl,
    write("*************************************"),nl,
    write("Wprowadz cyfre {1, 2, 3}: "),
    read(Choice),nl,
    (
        Choice == 1 -> user_process;
        Choice == 2 -> expert:start_expert_module;
        Choice == 3 -> !, halt
    ),
    halt.

user_process :- 
    write("Zostaniesz przeprowadzony przez serie pytan niezbednych do przeprowadzenia diagnozy pacjenta"),nl,
    ask_questions(Answers),
    diagnose(Answers, DiseaseAndProbability),
    show_results(DiseaseAndProbability).

:-
    [expert],
    [interview],
    [diagnosis],
    [helpers],
    menu.
