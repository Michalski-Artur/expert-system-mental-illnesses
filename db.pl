:- module(db, []).

% % 1 - nastroj
:- dynamic mood/2.

mood(personality_disorders, 0.4).
mood(affective_disorders, 0.3).
mood(schizophrenia, 0.6).
mood(depression, 0.1).
mood(nutritional_disorders, 0.4).
mood(anxiety_disorders, 0.3).

% 2 - problemy ze snem
:- dynamic sleep_problems/2.

sleep_problems(personality_disorders, nie).
sleep_problems(affective_disorders, nie).
sleep_problems(schizophrenia, tak).
sleep_problems(depression, nie).
sleep_problems(nutritional_disorders, nie).
sleep_problems(anxiety_disorders, tak).


% 3 - mysli samobojcze
:- dynamic suicidal_thoughts/2.

suicidal_thoughts(personality_disorders,nie).
suicidal_thoughts(affective_disorders,tak).
suicidal_thoughts(schizophrenia,tak).
suicidal_thoughts(depression,tak).
suicidal_thoughts(nutritional_disorders,tak).
suicidal_thoughts(anxiety_disorders,nie).


% 4 - halucynacje wzrokowe
:- dynamic visual_hallucinations/2.

visual_hallucinations(personality_disorders,rzadko).
visual_hallucinations(affective_disorders,rzadko).
visual_hallucinations(schizophrenia,czesto).
visual_hallucinations(depression,brak).
visual_hallucinations(nutritional_disorders,brak).
visual_hallucinations(anxiety_disorders,brak).

% 5 - halucynacje sluchowe
:- dynamic auditory_hallucinations/2.

auditory_hallucinations(personality_disorders,brak).
auditory_hallucinations(affective_disorders,rzadko).
auditory_hallucinations(schizophrenia,czesto).
auditory_hallucinations(depression,brak).
auditory_hallucinations(nutritional_disorders,brak).
auditory_hallucinations(anxiety_disorders,czesto).

% 6 - uzaleznienia

addictions(personality_disorders,nie).
addictions(affective_disorders,tak).
addictions(schizophrenia,tak).
addictions(depression,tak).
addictions(nutritional_disorders,tak).
addictions(anxiety_disorders,nie).


% 7 - natezenie stresu
:- dynamic stress_intensity/2.

stress_intensity(personality_disorders,0.15).
stress_intensity(affective_disorders,0.3).
stress_intensity(schizophrenia,0.7).
stress_intensity(depression,0.3).
stress_intensity(nutritional_disorders,1.0).
stress_intensity(anxiety_disorders,0.0).

% 8 - poczucie odmiennosci
:- dynamic sense_of_difference/2.

sense_of_difference(personality_disorders,0.85).
sense_of_difference(affective_disorders,0.6).
sense_of_difference(schizophrenia,0.85).
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

difficulty_focusing_attention(personality_disorders,nie).
difficulty_focusing_attention(affective_disorders,tak).
difficulty_focusing_attention(schizophrenia,tak).
difficulty_focusing_attention(depression,nie).
difficulty_focusing_attention(nutritional_disorders,tak).
difficulty_focusing_attention(anxiety_disorders,tak).

% 11 - ataki paniki
:- dynamic panic_attacks/2.

panic_attacks(personality_disorders,nie).
panic_attacks(affective_disorders,tak).
panic_attacks(schizophrenia,tak).
panic_attacks(depression,nie).
panic_attacks(nutritional_disorders,tak).
panic_attacks(anxiety_disorders,nie).

% 12 - brak zaufania
:- dynamic lack_of_trust/2.

lack_of_trust(personality_disorders,0.4).
lack_of_trust(affective_disorders,0.7).
lack_of_trust(schizophrenia,0.8).
lack_of_trust(depression,0.15).
lack_of_trust(nutritional_disorders,0.4).
lack_of_trust(anxiety_disorders,0.15).


