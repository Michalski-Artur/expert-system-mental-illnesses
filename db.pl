
% 1 - nastroj
:- dynamic mood/2.

mood(personality_disorders, 0.7).
mood(affective_disorders, 0.2).
mood(schizophrenia, 0.9).
mood(depression, 0.1).
mood(nutritional_disorders, 0.4).
mood(anxiety_disorders, 0.3).

% 2 - problemy ze snem
:- dynamic sleep_problems/2.

sleep_problems(personality_disorders, no).
sleep_problems(affective_disorders, no).
sleep_problems(schizophrenia, yes).
sleep_problems(depression, no).
sleep_problems(nutritional_disorders, no).
sleep_problems(anxiety_disorders, yes).


% 3 - mysli samobojcze
:- dynamic suicidal_thoughts/2.

suicidal_thoughts(personality_disorders,no).
suicidal_thoughts(affective_disorders,yes).
suicidal_thoughts(schizophrenia,yes).
suicidal_thoughts(depression,yes).
suicidal_thoughts(nutritional_disorders,yes).
suicidal_thoughts(anxiety_disorders,no).


% 4 - halucynacje wzrokowe
:- dynamic visual_hallucinations/2.

visual_hallucinations(personality_disorders,rarely).
visual_hallucinations(affective_disorders,rarely).
visual_hallucinations(schizophrenia,often).
visual_hallucinations(depression,none).
visual_hallucinations(nutritional_disorders,none).
visual_hallucinations(anxiety_disorders,none).

% 5 - halucynacje sluchowe
:- dynamic auditory_hallucinations/2.

auditory_hallucinations(personality_disorders,none).
auditory_hallucinations(affective_disorders,rarely).
auditory_hallucinations(schizophrenia,often).
auditory_hallucinations(depression,none).
auditory_hallucinations(nutritional_disorders,none).
auditory_hallucinations(anxiety_disorders,rarely).

% 6 - uzaleznienia

addictions(personality_disorders,no).
addictions(affective_disorders,yes).
addictions(schizophrenia,yes).
addictions(depression,yes).
addictions(nutritional_disorders,yes).
addictions(anxiety_disorders,no).


% 7 - natezenie stresu
:- dynamic stress_intensity/2.

stress_intensity(personality_disorders,0.15).
stress_intensity(affective_disorders,0.5).
stress_intensity(schizophrenia,0.92).
stress_intensity(depression,0.3).
stress_intensity(nutritional_disorders,1.0).
stress_intensity(anxiety_disorders,0.1).

% 8 - poczucie odmiennosci
:- dynamic sense_of_difference.

sense_of_difference(personality_disorders,0.92).
sense_of_difference(affective_disorders,0.6).
sense_of_difference(schizophrenia,0.75).
sense_of_difference(depression,0.3).
sense_of_difference(nutritional_disorders,0.1).
sense_of_difference(anxiety_disorders,0.4).


% 9 - zmiana wagi ciala
:- dynamic change_in_bodyweight/2.

change_in_bodyweight(personality_disorders,0.5).
change_in_bodyweight(affective_disorders,0.45).
change_in_bodyweight(schizophrenia,0.47).
change_in_bodyweight(depression,0.3).
change_in_bodyweight(nutritional_disorders,0.1).
change_in_bodyweight(anxiety_disorders,0.5).

% 10 - trudnosc w skupieniu uwagi
:- dynamic difficulty_focusing_attention/2.

difficulty_focusing_attention(personality_disorders,no).
difficulty_focusing_attention(affective_disorders,yes).
difficulty_focusing_attention(schizophrenia,yes).
difficulty_focusing_attention(depression,no).
difficulty_focusing_attention(nutritional_disorders,yes).
difficulty_focusing_attention(anxiety_disorders,yes).

% 11 - ataki paniki
:- dynamic panic_attacks/2.

panic_attacks(personality_disorders,no).
panic_attacks(affective_disorders,yes).
panic_attacks(schizophrenia,yes).
panic_attacks(depression,no).
panic_attacks(nutritional_disorders,yes).
panic_attacks(anxiety_disorders,no).

% 12 - brak zaufania
:- dynamic lack_of_trust/2.

lack_of_trust(personality_disorders,0.5).
lack_of_trust(affective_disorders,0.1).
lack_of_trust(schizophrenia,0.7).
lack_of_trust(depression,0.75).
lack_of_trust(nutritional_disorders,0.4).
lack_of_trust(anxiety_disorders,0.1).


