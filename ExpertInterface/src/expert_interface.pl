:- module(expert_interface, [expert_menu/0]).

:- use_module(variables).

expert_menu :-
	format('~nWhat do you want to do?~n~n'),
	print_variables([insert, update, delete]),
	read(Opt),
	(
			Opt == insert -> insert_interface;
	    Opt == update -> update_interface;
	    Opt == delete -> delete_interface
	).


update_interface :-
  	format('~nYou are updating existing illness.~n'),
  	read_illness(Illness),
	read_answers(Answers),
	format('~nDo you really want to update the illness?~n'),
	confirm,
  	update_illness(Illness, Answers),
  	format('~nIllness was successfully updated.~n').


insert_interface :-
    format('~nYou are inserting new illness.~n'),
    read_illness(Illness),
	read_answers(Answers),
	format('~nDo you really want to add the illness?~n'),
	confirm,
    insert_illness(Illness, Answers),
    format('~nIllness was successfully inserted.~n').

delete_interface :-
    format('~nYou are deleting existing illness.~n'),
    read_illness(Illness),
	format('~nDo you really want to delete the illness?~n'),
	confirm,
		delete_illness(Illness),
		format('~nIllness was successfully deleted.~n').


read_Illness(Name) :-
    format('~nWhat is illness name?~n'),
	read(Name).

confirm :-
    read_boolean(Opt),
	(
		Opt == yes -> true;
	    format('~nOperation cancelled.~n'), false
	).
