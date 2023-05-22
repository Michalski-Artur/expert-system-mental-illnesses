:- module(main, [main/0]).

use_module(pytania).
use_module(diagnoza).
use_module(helpers).
use_module(ekspert).

main:-
    write("*************************************"),nl,
    write("Politechnika Warszawska MiNI"),nl,
    write("Systemy Ekspertowe, rozpoznawanie chorób psychicznych."),nl,
    write("Piotr Brysiak"),nl,
    write("Artur Michalski"),nl,
    write("Jeremi Kurdek"),nl,
    write("Tymek Perka"),nl,
    repeat,
    write("*************************************"),nl,
    write("* 1. Wykonaj wywiad z pacjentem."),nl,
    write("* 2. Uruchom modul eksperta."),nl,
    write("* 3. Zamknij aplikacje."),nl,
    write("*************************************"),nl,
    write("Wprowadz cyfre {1, 2, 3}: "),
    read(Choice),nl,
    (
        Choice == 1 -> user_process;
        Choice == 2 -> ekspert:start_expert_module;
        Choice == 3 -> !, halt
    ),
    fail.

user_process :- 
    write("Zostaniesz przeprowadzony przez serie pytan niezbednych do przeprowadzenia diagnozy pacjenta"),nl,
    ask_questions(Answers),
    diagnoza(Answers, DiseaseAndProbability),
    show_results(DiseaseAndProbability).

:-
    [ekspert],
    [pytania],
    [diagnoza],
    [helpers],
    main.
