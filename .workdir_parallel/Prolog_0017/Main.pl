:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readutil)).

main :-
    read_line_to_string(user_input, S),
    string_chars(S, Chars),
    longest_unique_substring(Chars, Length),
    writeln(Length),
    halt.

longest_unique_substring(Chars, MaxLength) :-
    longest_unique_substring(Chars, 0, 0, [], MaxLength).

longest_unique_substring([], _, MaxSoFar, _, MaxSoFar).
longest_unique_substring([C|Rest], Start, MaxSoFar, Window, MaxLength) :-
    (   member(C, Window)
    ->  remove_until(Window, C, NewWindow),
        append(NewWindow, [C], UpdatedWindow),
        length(UpdatedWindow, CurrentLen),
        NewMax is max(MaxSoFar, CurrentLen),
        longest_unique_substring(Rest, Start, NewMax, UpdatedWindow, MaxLength)
    ;   append(Window, [C], UpdatedWindow),
        length(UpdatedWindow, CurrentLen),
        NewMax is max(MaxSoFar, CurrentLen),
        longest_unique_substring(Rest, Start, NewMax, UpdatedWindow, MaxLength)
    ).

remove_until([], _, []).
remove_until([C|Rest], C, Rest) :- !.
remove_until([_|Rest], C, Result) :-
    remove_until(Rest, C, Result).

:- initialization(main).
