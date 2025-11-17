:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readutil)).

main :-
    read_line_to_string(user_input, M_Line),
    number_string(M, M_Line),
    read_entries(M, Entries),
    read_line_to_string(user_input, Q_Line),
    number_string(Q, Q_Line),
    process_queries(Q, Entries),
    halt.

read_entries(0, []) :- !.
read_entries(N, [Key-Value|Rest]) :-
    N > 0,
    read_line_to_string(user_input, Line),
    split_string(Line, " ", "", [KeyStr, ValueStr]),
    atom_string(Key, KeyStr),
    atom_string(Value, ValueStr),
    N1 is N - 1,
    read_entries(N1, Rest).

process_queries(0, _) :- !.
process_queries(N, Entries) :-
    N > 0,
    read_line_to_string(user_input, Line),
    atom_string(Key, Line),
    (member(Key-Value, Entries) -> writeln(Value) ; writeln('NA')),
    N1 is N - 1,
    process_queries(N1, Entries).

:- main.
