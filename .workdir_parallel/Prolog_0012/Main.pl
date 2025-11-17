:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readutil)).

run_length_encode([], []).
run_length_encode([H|T], Result) :-
    count_char(H, [H|T], Count, Rest),
    run_length_encode(Rest, RestResult),
    atom_chars(Count, CountChars),
    append([H|CountChars], RestResult, Result).

count_char(_, [], 0, []).
count_char(C, [C|T], Count, Rest) :-
    !,
    count_char(C, T, Count1, Rest),
    Count is Count1 + 1.
count_char(_, L, 0, L).

main :-
    read_line_to_string(user_input, Line),
    (Line \= end_of_file ->
        string_chars(Line, Chars),
        run_length_encode(Chars, Encoded),
        atom_chars(Output, Encoded),
        write(Output), nl
    ; true),
    halt.

:- main.
