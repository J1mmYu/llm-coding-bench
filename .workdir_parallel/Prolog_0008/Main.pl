:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readutil)).

main :-
    read_line_to_string(user_input, Line1),
    number_string(N, Line1),
    read_intervals(N, Intervals),
    sort(Intervals, Sorted),
    merge_intervals(Sorted, Merged),
    total_length(Merged, Total),
    writeln(Total),
    halt.

read_intervals(0, []) :- !.
read_intervals(N, [[L,R]|Rest]) :-
    N > 0,
    read_line_to_string(user_input, Line),
    split_string(Line, " ", " ", Parts),
    maplist(number_string, [L, R], Parts),
    N1 is N - 1,
    read_intervals(N1, Rest).

merge_intervals([], []) :- !.
merge_intervals([[L,R]], [[L,R]]) :- !.
merge_intervals([[L1,R1],[L2,R2]|Rest], Merged) :-
    L2 =< R1,
    !,
    MaxR is max(R1, R2),
    merge_intervals([[L1,MaxR]|Rest], Merged).
merge_intervals([[L1,R1]|Rest], [[L1,R1]|Merged]) :-
    merge_intervals(Rest, Merged).

total_length([], 0).
total_length([[L,R]|Rest], Total) :-
    total_length(Rest, RestTotal),
    Total is RestTotal + (R - L).

:- initialization(main).
