:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

main :-
    read_line_to_string(user_input, Line),
    (valid_ipv4(Line) -> write('1') ; write('0')),
    nl,
    halt.

valid_ipv4(String) :-
    split_string(String, ".", "", Parts),
    length(Parts, 4),
    maplist(valid_octet, Parts).

valid_octet(String) :-
    string_length(String, Len),
    Len > 0,
    Len =< 3,
    atom_string(Atom, String),
    atom_number(Atom, Num),
    integer(Num),
    Num >= 0,
    Num =< 255,
    \+ leading_zero(String, Num).

leading_zero(String, Num) :-
    string_length(String, Len),
    Len > 1,
    sub_string(String, 0, 1, _, "0"),
    Num >= 0.
