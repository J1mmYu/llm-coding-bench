:- set_stream(user_input, encoding(utf8)).
:- set_stream(user_output, encoding(utf8)).

read_lines(Lines) :-
    read_line_to_string(user_input, Line),
    (   Line == end_of_file
    ->  Lines = []
    ;   Lines = [Line|Rest],
        read_lines(Rest)
    ).

distinct_chars(String, Count) :-
    string_chars(String, Chars),
    list_to_set(Chars, UniqueChars),
    length(UniqueChars, Count).

find_max_line([], _, _, Result, Result).
find_max_line([Line|Rest], MaxCount, MaxLine, _, Result) :-
    distinct_chars(Line, Count),
    Count > MaxCount,
    !,
    find_max_line(Rest, Count, Line, Line, Result).
find_max_line([_|Rest], MaxCount, MaxLine, _, Result) :-
    find_max_line(Rest, MaxCount, MaxLine, MaxLine, Result).

main :-
    read_lines(Lines),
    (   Lines = []
    ->  true
    ;   Lines = [First|Rest],
        distinct_chars(First, FirstCount),
        find_max_line(Rest, FirstCount, First, First, MaxLine),
        writeln(MaxLine)
    ).

:- initialization(main, main).
