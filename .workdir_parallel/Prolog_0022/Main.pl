:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

main :-
    read_line_to_string(user_input, Line),
    (   Line \= end_of_file
    ->  filter_alphanumeric(Line, Filtered),
        string_lower(Filtered, Lower),
        string_chars(Lower, Chars),
        (   is_palindrome(Chars)
        ->  writeln('YES')
        ;   writeln('NO')
        )
    ;   true
    ),
    halt.

filter_alphanumeric(String, Filtered) :-
    string_chars(String, Chars),
    include(is_alnum, Chars, FilteredChars),
    string_chars(Filtered, FilteredChars).

is_alnum(C) :-
    char_type(C, alnum).

is_palindrome(List) :-
    reverse(List, List).
