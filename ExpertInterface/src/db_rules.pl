:- module(db_rules, [illness_is/1]). 

:-use_module(engine_interface). 

illness_is(test) :- 
( 
make_sure(sleep_problems,     no) ; 
make_sure(suicidal_thoughts,     no) ; 
make_sure(stress_intensity,     low) ; 
make_sure(sense_of_difference,     low) ; 
make_sure(change_in_body_weight,     low) ; 
make_sure(difficulty_focusing_attention,     no) ; 
make_sure(panic_attacks,     no) ; 
make_sure(lack_of_trust,     low) 
). 

illness_is(schizophrenia) :- 
( 
make_sure(sleep_problems,     yes) ; 
make_sure(suicidal_thoughts,     yes) ; 
make_sure(stress_intensity,     high) ; 
make_sure(sense_of_difference,     high) ; 
make_sure(change_in_body_weight,     medium) ; 
make_sure(difficulty_focusing_attention,     yes) ; 
make_sure(panic_attacks,     yes) ; 
make_sure(lack_of_trust,     high) 
). 

illness_is(test2) :- 
( 
make_sure(mood,    low) ; 
make_sure(sleep_problems,    no) ; 
make_sure(suicidal_thoughts,    no) ; 
make_sure(visual_hallucinations,    no) ; 
make_sure(auditory_hallucinations,    no) ; 
make_sure(addictions,    no) ; 
make_sure(stress_intensity,    low) ; 
make_sure(sense_of_difference,    low) ; 
make_sure(change_in_body_weight,    low) ; 
make_sure(difficulty_focusing_attention,    no) ; 
make_sure(panic_attacks,    no) ; 
make_sure(lack_of_trust,    low) 
). 

illness_is(test3) :- 
( 
make_sure(mood,   medium) ; 
make_sure(sleep_problems,   yes) ; 
make_sure(suicidal_thoughts,   yes) ; 
make_sure(visual_hallucinations,   rarely) ; 
make_sure(auditory_hallucinations,   rarely) ; 
make_sure(addictions,   yes) ; 
make_sure(stress_intensity,   medium) ; 
make_sure(sense_of_difference,   medium) ; 
make_sure(change_in_body_weight,   medium) ; 
make_sure(difficulty_focusing_attention,   yes) ; 
make_sure(panic_attacks,   yes) ; 
make_sure(lack_of_trust,   medium) 
). 

