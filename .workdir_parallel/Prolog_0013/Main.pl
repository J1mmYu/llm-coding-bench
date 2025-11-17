:- set_prolog_flag(encoding, utf8).
:- set_prolog_flag(double_quotes, chars).

read_line(Chars) :-
    read_line_to_string(user_input, String),
    string_chars(String, Chars).

char_to_digit(C, D) :- char_code(C, Code), D is Code - 48.

digits_to_number(Digits, Number) :-
    reverse(Digits, Rev),
    digits_to_number_acc(Rev, 0, Number).

digits_to_number_acc([], Acc, Acc).
digits_to_number_acc([D|Ds], Acc, Number) :-
    NewAcc is Acc * 10 + D,
    digits_to_number_acc(Ds, NewAcc, Number).

add_digits([], [], 0, []).
add_digits([], [], Carry, [Carry]) :- Carry > 0.
add_digits([D1|D1s], [], Carry, Result) :-
    add_digits([D1|D1s], [0], Carry, Result).
add_digits([], [D2|D2s], Carry, Result) :-
    add_digits([0], [D2|D2s], Carry, Result).
add_digits([D1|D1s], [D2|D2s], Carry, [R|Rs]) :-
    Sum is D1 + D2 + Carry,
    R is Sum mod 10,
    NewCarry is Sum // 10,
    add_digits(D1s, D2s, NewCarry, Rs).

print_digits([]).
print_digits([D|Ds]) :-
    print_digits(Ds),
    write(D).

main :-
    read_line(Line1),
    read_line(Line2),
    maplist(char_to_digit, Line1, Digits1),
    maplist(char_to_digit, Line2, Digits2),
    reverse(Digits1, Rev1),
    reverse(Digits2, Rev2),
    add_digits(Rev1, Rev2, 0, ResultRev),
    reverse(ResultRev, Result),
    print_digits(Result),
    nl,
    halt.

:- initialization(main).
