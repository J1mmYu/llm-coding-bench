:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readutil)).
:- use_module(library(lists)).

split_words(Line, Words) :-
    split_string(Line, " \t\n\r", " \t\n\r", Strings),
    maplist(atom_string, Words, Strings).

count_words([], []).
count_words([W|Ws], Counts) :-
    count_words(Ws, RestCounts),
    (   select(W-N, RestCounts, Rest)
    ->  N1 is N + 1,
        Counts = [W-N1|Rest]
    ;   Counts = [W-1|RestCounts]
    ).

compare_entries(Order, W1-C1, W2-C2) :-
    (   C1 > C2
    ->  Order = (<)
    ;   C1 < C2
    ->  Order = (>)
    ;   compare(Order, W1, W2)
    ).

main :-
    read_line_to_string(user_input, Line),
    (   Line \= end_of_file
    ->  split_words(Line, Words),
        count_words(Words, Counts),
        predsort(compare_entries, Counts, Sorted),
        forall(member(Word-Count, Sorted),
               format('~w ~w~n', [Word, Count]))
    ;   true
    ),
    halt.

:- initialization(main).
