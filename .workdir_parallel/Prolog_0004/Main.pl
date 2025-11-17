:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readln)).

main :-
    read_line_to_string(user_input, Line),
    string_chars(Line, Chars),
    (check_balanced(Chars, []) -> writeln('YES') ; writeln('NO')),
    halt.

check_balanced([], []).
check_balanced([C|Rest], Stack) :-
    (opening(C) ->
        check_balanced(Rest, [C|Stack])
    ; closing(C) ->
        (Stack = [Top|NewStack],
         matches(Top, C) ->
            check_balanced(Rest, NewStack)
        ; fail)
    ; check_balanced(Rest, Stack)).

opening('(').
opening('[').
opening('{').

closing(')').
closing(']').
closing('}').

matches('(', ')').
matches('[', ']').
matches('{', '}').

:- initialization(main).
