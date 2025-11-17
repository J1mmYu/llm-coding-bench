:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readutil)).

main :-
    read_line_to_string(user_input, Line1),
    number_string(N, Line1),
    read_line_to_string(user_input, Line2),
    split_string(Line2, " ", "", Parts),
    maplist(number_string, Numbers, Parts),
    list_to_set(Numbers, Set),
    length(Set, Count),
    writeln(Count),
    halt.

:- initialization(main).
