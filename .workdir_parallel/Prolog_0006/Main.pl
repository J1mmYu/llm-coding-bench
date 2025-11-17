:- set_prolog_flag(double_quotes, codes).
:- initialization(main, main).

main :-
    read_line_to_codes(user_input, Codes),
    (Codes == end_of_file -> halt(0) ; true),
    parse_csv_line(Codes, Fields),
    length(Fields, Count),
    write(Count), nl,
    halt(0).

parse_csv_line(Codes, Fields) :-
    parse_fields(Codes, Fields).

parse_fields([], []) :- !.
parse_fields(Codes, [Field|Fields]) :-
    parse_field(Codes, Field, Rest),
    (Rest = [] -> Fields = [] ;
     Rest = [44|RestCodes] -> parse_fields(RestCodes, Fields) ;
     Fields = []).

parse_field([34|Codes], Field, Rest) :- !,
    parse_quoted_field(Codes, FieldCodes, Rest),
    atom_codes(Field, FieldCodes).
parse_field(Codes, Field, Rest) :-
    parse_unquoted_field(Codes, FieldCodes, Rest),
    atom_codes(Field, FieldCodes).

parse_quoted_field([], [], []) :- !.
parse_quoted_field([34,34|Codes], [34|FieldCodes], Rest) :- !,
    parse_quoted_field(Codes, FieldCodes, Rest).
parse_quoted_field([34|Codes], [], Codes) :- !.
parse_quoted_field([C|Codes], [C|FieldCodes], Rest) :-
    parse_quoted_field(Codes, FieldCodes, Rest).

parse_unquoted_field([], [], []) :- !.
parse_unquoted_field([44|Codes], [], [44|Codes]) :- !.
parse_unquoted_field([C|Codes], [C|FieldCodes], Rest) :-
    parse_unquoted_field(Codes, FieldCodes, Rest).
