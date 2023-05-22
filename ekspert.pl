:- module(ekspert, [start_expert_module/0]).

:- use_module(db).

start_expert_module :-
    repeat,
    write("***************************"),nl,
    write("* 1. Zaktualizuj chorobe."),nl,
    write("* 2. Dodaj chorobe."),nl,
    write("* 3. Usun chorobe."),nl,
    write("* 4. Wyswietl zapisane choroby."),nl,
    write("* 5. Zamknij modul eksperta."),nl,
    write("****************************"),nl,
    write("Wprowadz cyfre {1, 2, 3, 4, 5}: "),
    read(Choice),nl,
    (
        Choice == 1 -> update_disease;
        Choice == 2 -> add_disease;
        Choice == 3 -> delete_disease;
        Choice == 4 -> show_disease;
        Choice == 5 -> !, fail
    ),
    fail.

show_disease:-
    findall(Fact, db:mood(Fact,_), Facts),
    write(Facts).

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
    tell('db.pl'),
    listing(db:mood),
    listing(db:sleep_problems),
    listing(db:suicidal_thoughts),
    listing(db:visual_hallucinations),
    listing(db:auditory_hallucinations),
    listing(db:addictions),
    listing(db:stress_intensity),
    listing(db:sense_of_difference),
    listing(db:change_in_bodyweight),
    listing(db:difficulty_focusing_attention),
    listing(db:panic_attacks),
    listing(db:lack_of_trust),
    told.

delete_entry(Disease) :-
    retractall(db:mood(Disease, _)),
    retractall(db:sleep_problems(Disease, _)),
    retractall(db:suicidal_thoughts(Disease, _)),
    retractall(db:visual_hallucinations(Disease, _)),
    retractall(db:auditory_hallucinations(Disease, _)),
    retractall(db:addictions(Disease, _)),
    retractall(db:stress_intensity(Disease, _)),
    retractall(db:sense_of_difference(Disease, _)),
    retractall(db:change_in_bodyweight(Disease, _)),
    retractall(db:difficulty_focusing_attention(Disease, _)),
    retractall(db:panic_attacks(Disease, _)),
    retractall(db:lack_of_trust(Disease, _)),
    save_changes.


get_symptoms([
    Mood,
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
        write("Samopoczucie [0-1]: "),
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
        read(Stress),

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
    Mood,
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
    assertz(db:mood(Disease,Mood)),
    assertz(db:sleep_problems(Disease,SleepProblem)),
    assertz(db:suicidal_thoughts(Disease,Suicide)),
    assertz(db:visual_hallucinations(Disease,Visual_hallucinations)),
    assertz(db:auditory_hallucinations(Disease,Sound_hallucinations)),
    assertz(db:addictions(Disease,Addiction)),
    assertz(db:stress_intensity(Disease,Stress)),
    assertz(db:sense_of_difference(Disease,Differ)),
    assertz(db:change_in_bodyweight(Disease,Weight_change)),
    assertz(db:difficulty_focusing_attention(Disease,Difficulty_concentrating)),
    assertz(db:panic_attacks(Disease,Panic_attacks)),
    assertz(db:lack_of_trust(Disease,Suspiciousness)),
    save_changes.
