:- module(diagnoza, [diagnoza/2]).

:- use_module(helpers).
:- use_module(analiza).
:- consult(redukty).

diagnoza(Answers,DiseaseAndProbability):-
    findall(
        Probability-Disease,
        get_disease_probability(Answers, Disease, Probability),NotSortedDiseaseAndProbability),
    keysort(NotSortedDiseaseAndProbability, SortedDiseaseAndProbability),
    reverse(SortedDiseaseAndProbability, DiseaseAndProbability).


get_disease_probability([M,SL,SU,VH,SH,A,S,D,WC,DC,PA,SUS], Disease, Probability) :-
    findall(
            R,
            reduct(R),
            Reducts
        ),

    get_mood_prob(M,Disease,M_prob),
	get_sleep_prob(SL,Disease,SL_prob),
	get_suicide_prob(SU,Disease,SU_prob),
	get_visual_hallucinations_prob(VH,Disease,VH_prob),
	get_sound_hallucinations_prob(SH,Disease,SH_prob),
    get_addiction_prob(A,Disease,A_prob),
    get_stress_prob(S,Disease,S_prob),
    get_differ_prob(D,Disease,D_prob),
	get_weight_change_prob(WC,Disease,WC_prob),
	get_difficulty_concentrating_prob(DC,Disease,DC_prob),
	get_panic_attacks_prob(PA,Disease,PA_prob),
	get_suspiciousness_prob(SUS,Disease,SUS_prob),
    Probabilities = [],    
    (
        member(mood, Reducts) -> get_mood_prob(M, Disease, M_prob), M_Probabilities = [M_prob|Probabilities] ; M_Probabilities = Probabilities
    ),
    (
        member(sleepProblem, Reducts) -> get_sleep_prob(SL, Disease, SL_prob), SL_Probabilities = [SL_prob|M_Probabilities] ; SL_Probabilities = M_Probabilities
    ),
    (
        member(suicide, Reducts) -> get_suicide_prob(SU, Disease, SU_prob), SU_Probabilities = [SU_prob|SL_Probabilities] ; SU_Probabilities = SL_Probabilities
    ),
    (
        member(visual_hallucinations, Reducts) -> get_visual_hallucinations_prob(VH, Disease, VH_prob), VH_Probabilities = [VH_prob|SU_Probabilities] ; VH_Probabilities = SU_Probabilities
    ),
    (
        member(sound_hallucinations, Reducts) -> get_sound_hallucinations_prob(SH, Disease, SH_prob), SH_Probabilities = [SH_prob|VH_Probabilities] ; SH_Probabilities = VH_Probabilities
    ),
    (
        member(addiction, Reducts) -> get_addiction_prob(A, Disease, A_prob), A_Probabilities = [A_prob|SH_Probabilities] ; A_Probabilities = SH_Probabilities
    ),
    (
        member(stress, Reducts) -> get_stress_prob(S, Disease, S_prob), S_Probabilities = [S_prob|A_Probabilities] ; S_Probabilities = A_Probabilities
    ),
    (
        member(differ, Reducts) -> get_differ_prob(D, Disease, D_prob), D_Probabilities = [D_prob|S_Probabilities] ; D_Probabilities = S_Probabilities
    ),
    (
        member(weight, Reducts) -> get_weight_prob(WC, Disease, WC_prob), WC_Probabilities = [WC_prob|D_Probabilities] ; WC_Probabilities = D_Probabilities
    ),
    (
        member(difficulty_concentrating, Reducts) -> get_difficulty_concentrating_prob(DC, Disease, DC_prob), DC_Probabilities = [DC_prob|WC_Probabilities] ; DC_Probabilities = WC_Probabilities
    ),
    (
        member(panic_attacks, Reducts) -> get_panic_attacks_prob(PA, Disease, PA_prob), PA_Probabilities = [PA_prob|DC_Probabilities] ; PA_Probabilities = DC_Probabilities
    ),
    (
        member(suspiciousness, Reducts) -> get_suspiciousness_prob(SUS, Disease, SUS_prob), SUS_Probabilities = [SUS_prob|PA_Probabilities] ; SUS_Probabilities = PA_Probabilities
    ),


    get_mean_value(SUS_Probabilities,Probability),
    true.


get_mood_prob(M,Disease,M_prob):-
    blur_fuzzy(M,M_blur),
    analyze_mood(Disease,Factor),
    M_prob is 1 - abs(M_blur - Factor).

get_sleep_prob(SL,Disease,SL_prob):-
    blur_yesno(SL,SL_blur),
    analyze_sleep_problems(Disease,Factor),
    SL_prob is 1 - abs(SL_blur - Factor).

get_suicide_prob(SU,Disease,SU_prob):-
    blur_yesno(SU,SU_blur),
    analyze_suicidal_thoughts(Disease,Factor),
    SU_prob is 1 - abs(SU_blur - Factor).

get_visual_hallucinations_prob(VH,Disease,VH_prob):-
    blur_freq(VH,VH_blur),
    analyze_visual_hallucinations(Disease,Factor),
    VH_prob is 1 - abs(VH_blur - Factor).

get_sound_hallucinations_prob(SH,Disease,SH_prob):-
    blur_freq(SH,SH_blur),
    analyze_auditory_hallucinations(Disease,Factor),
    SH_prob is 1 - abs(SH_blur - Factor).

get_addiction_prob(A,Disease,A_prob):-
    blur_yesno(A,A_blur),
    analyze_addictions(Disease,Factor),
    A_prob is 1 - abs(A_blur - Factor).

get_stress_prob(S,Disease,S_prob):-
    blur_fuzzy(S,S_blur),
    analyze_stress_intensity(Disease,Factor),
    S_prob is 1 - abs(S_blur - Factor).

get_differ_prob(D,Disease,D_prob):-
    blur_fuzzy(D,D_blur),
    analyze_difference(Disease,Factor),
    D_prob is 1 - abs(D_blur - Factor).

get_weight_change_prob(WC,Disease,WC_prob):-
    blur_fuzzy(WC,WC_blur),
    analyze_change_in_bodyweight(Disease,Factor),
    WC_prob is 1 - abs(WC_blur - Factor).

get_difficulty_concentrating_prob(DC,Disease,DC_prob):-
    blur_yesno(DC,DC_blur),
    analyze_difficulty_focusing(Disease,Factor),
    DC_prob is 1 - abs(DC_blur - Factor).
	
get_panic_attacks_prob(PA,Disease,PA_prob):-
    blur_yesno(PA,PA_blur),
    analyze_panic_attacks(Disease,Factor),
    PA_prob is 1 - abs(PA_blur - Factor).

get_suspiciousness_prob(SUS,Disease,SUS_prob):-
    blur_fuzzy(SUS,SUS_blur),
    analyze_lack_of_trust(Disease,Factor),
    SUS_prob is 1 - abs(SUS_blur - Factor).

:-
	[helpers],
    [analiza].
