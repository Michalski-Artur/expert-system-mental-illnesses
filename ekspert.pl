:- module(ekspert, [start_expert_module/0]).

:- use_module(database).

start_expert_module :-
    repeat,
    write("***************************"),nl,
    write("* 1. Zaktualizuj chorobe."),nl,
    write("* 2. Dodaj chorobe."),nl,
    write("* 3. Usun chorobe."),nl,
    write("* 4. Zamknij modul eksperta."),nl,
    write("****************************"),nl,
    write("Wprowadz cyfre {1, 2, 3, 4}: "),
    read(Choice),nl,
    (
        Choice == 1 -> update_disease;
        Choice == 2 -> add_disease;
        Choice == 3 -> delete_disease;
        Choice == 4 -> !, fail
    ),
    fail.

update_disease :- 
    write("*** Aktualizacja choroby ***"), nl,
    get_disease_name(Disease),
    get_symptoms(Symptoms),
    delete_entry(Disease),
    add_entry(Disease, Symptoms),
    write("Choroba zostala zaktualizowana."), nl.

add_disease :- 
    write("*** Dodanie choroby ***"), nl,
    get_disease_name(Disease),
    get_symptoms(Symptoms),
    add_entry(Disease, Symptoms),
    write("Choroba zostala dodana."), nl.

delete_disease :- 
    write("*** Usunecie choroby ***"), nl,
    get_disease_name(Disease),
    delete_entry(Disease),
    write("Choroba zostala usunieta."), nl.

get_disease_name(Disease) :-
    write("Podaj nazwe choroby: "),
    read(Disease).

save_changes :-
    tell('database.pl'),
    listing(database:visual_hallucinations),

    listing(database:hearing_hallucinations),

    
    listing(database:somatic_hallucinations),
    listing(database:thinking_disorder),
    listing(database:high_stress),
    listing(database:communicativeness),
    listing(database:weight_change),
    listing(database:difficulty_concentrating),
    listing(database:panic_attacks),
    listing(database:suspiciousness),
    told.

delete_entry(Disease) :-
    retractall(database:visual_hallucinations(Disease, _)),
    retractall(database:hearing_hallucinations(Disease, _)),
    retractall(database:somatic_hallucinations(Disease, _)),
    retractall(database:thinking_disorder(Disease, _)),
    retractall(database:high_stress(Disease, _)),
    retractall(database:communicativeness(Disease, _)),
    retractall(database:weight_change(Disease, _)),
    retractall(database:difficulty_concentrating(Disease, _)),
    retractall(database:panic_attacks(Disease, _)),
    retractall(database:suspiciousness(Disease, _)),
    save_changes.

get_symptoms([
    Mood
    SleepProblem,
    Suicide,
    Visual_hallucinations,
    Sound_hallucinations,
    Addiction,
    Stress,
    Differ,
    Weight_change,
    Difficulty_concentrating,
    Panic_attacks,
    Suspiciousness
    ]) :-
        write("Samopoczucie {0, 0.5, 1}: "),
        read(Mood),

        write("Problemy ze snem {nie, tak}: "),
        read(SleepProblem),

        write("Mysli samobojcze {nie, tak}: "),
        read(Suicide),

        write("Halucynacje wzrokowe {brak, rzadko, czesto}: "),
        read(Visual_hallucinations),

        write("Halucynacje sluchowe {brak, rzadko, czesto}: "),
        read(Sound_hallucinations),

        write("Uzaleznienia {nie, tak}: "),
        read(Addiction),

        write("Stres [0 - 1]: "),
        read(Stres),

        write("Poczucie odmiennosci [0 - 1]: "),
        read(Differ),

        write("Zmiana wagi [0 - 1]: "),
        read(Weight_change),

        write("Problemy z koncetracja {nie,tak}: "),
        read(Difficulty_concentrating),

        write("Ataki paniki {nie,tak}: "),
        read(Panic_attacks),

        write("Brak zaufania [0 - 1]: "),
        read(Suspiciousness).

add_entry(Disease, [
    Visual_hallucinations, 
    Hearing_hallucinations,
    Somatic_hallucinations,
    Thinking_disorder,
    High_stress,
    Communicativeness,
    Weight_change,
    Difficulty_concentrating,
    Panic_attacks,
    Suspiciousness
    ]) :- 
    assertz(database:visual_hallucinations(Disease, Visual_hallucinations)),
    assertz(database:hearing_hallucinations(Disease, Hearing_hallucinations)),
    assertz(database:somatic_hallucinations(Disease, Somatic_hallucinations)),
    assertz(database:thinking_disorder(Disease, Thinking_disorder)),
    assertz(database:high_stress(Disease, High_stress)),
    assertz(database:communicativeness(Disease, Communicativeness)),
    assertz(database:weight_change(Disease, Weight_change)),
    assertz(database:difficulty_concentrating(Disease, Difficulty_concentrating)),
    assertz(database:panic_attacks(Disease, Panic_attacks)),
    assertz(database:suspiciousness(Disease, Suspiciousness)),
    save_changes.
