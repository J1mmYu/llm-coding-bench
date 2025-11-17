:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

main :-
    read_line_to_string(user_input, Line),
    string_concat(Line, "", Str),
    atom_number(Str, N),
    abs(N, AbsN),
    number_codes(AbsN, Codes),
    sum_digits(Codes, Sum),
    writeln(Sum),
    halt.

sum_digits([], 0).
sum_digits([H|T], Sum) :-
    sum_digits(T, RestSum),
    Digit is H - 48,
    Sum is Digit + RestSum.
