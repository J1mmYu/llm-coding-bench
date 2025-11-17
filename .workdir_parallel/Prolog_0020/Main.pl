:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readln)).

main :-
    read_line_to_string(user_input, Line),
    split_string(Line, ", ", ", ", Parts),
    maplist(number_string, Numbers, Parts),
    sum_list(Numbers, Sum),
    writeln(Sum),
    halt.

:- initialization(main).
