:- module(variables, [print_variables/1, available_options/2, question_text/2, illness_text/2]).

print_variables([]) :- format('~n').
print_variables([Head|Rest]) :-
	(
		Rest == [] -> format('~w',Head);
		format('~w, ',Head)
	),
	print_variables(Rest).

available_options(sleep_problems, [yes, no]).
available_options(suicidal_thoughts, [yes, no]).
available_options(visual_hallucinations, [no, rarely, often]).
available_options(auditory_hallucinations, [no, rarely, often]).
available_options(addictions, [yes, no]).
available_options(difficulty_focusing_attention, [no, yes]).
available_options(panic_attacks, [no, yes]).

question_text(mood, "How would you rate your mood (from 0 to 10)?\n").
question_text(sleep_problems, "Do you experience sleep problems?\n").
question_text(suicidal_thoughts, "Do you have suicidal thoughts?\n").
question_text(visual_hallucinations, "How often do you experience visual hallucinations?\n").
question_text(auditory_hallucinations, "How often do you experience auditory hallucinations?\n").
question_text(addictions, "Do you have any addictions?\n").
question_text(stress_intensity, "How would you rate intensity of the stress you experience (from 0 to 10)?\n").
question_text(sense_of_difference, "Do you have a sense of difference (from 0 to 10)?\n").
question_text(current_weight, "What is your current weight (in kg)?\n").
question_text(previous_weight, "What was your weight year ago (in kg)?\n").
question_text(difficulty_focusing_attention, "Do you experience difficulties focusing attention?\n").
question_text(panic_attacks, "Do you experience panic attacks?\n").
question_text(lack_of_trust, "How would you rate your trust in the doctor (from 0 to 10)?\n").

illness_text(schizophrenia, "Potential illness may be schizophrenia.\n").
illness_text(depression, "Potential illness may be depression.\n").
illness_text(affective_disorders, "Potential illness may be affective disorders, bipolar.\n").
illness_text(anxiety_disorders, "Potential illness may be anxiety and stress-related disorders.\n").
illness_text(personality_disorders, "Potential illness may be personality disorders.\n").
illness_text(nutritional_disorders, "Potential illness may be nutritional disorders.\n").
