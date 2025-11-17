:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

main :-
    read_line_to_string(user_input, Line),
    string_chars(Line, Chars),
    phrase(expr(Result), Chars, []),
    writeln(Result),
    halt.

expr(Result) --> term(T), expr_rest(T, Result).
expr_rest(Acc, Result) --> ['+'], !, term(T), { Acc1 is Acc + T }, expr_rest(Acc1, Result).
expr_rest(Acc, Result) --> ['-'], !, term(T), { Acc1 is Acc - T }, expr_rest(Acc1, Result).
expr_rest(Result, Result) --> [].

term(Result) --> factor(F), term_rest(F, Result).
term_rest(Acc, Result) --> ['*'], !, factor(F), { Acc1 is Acc * F }, term_rest(Acc1, Result).
term_rest(Acc, Result) --> ['/'], !, factor(F), { Acc1 is Acc / F }, term_rest(Acc1, Result).
term_rest(Result, Result) --> [].

factor(Result) --> ['('], !, expr(Result), [')'].
factor(N) --> digit(D), number(D, N).

number(Acc, N) --> digit(D), !, { Acc1 is Acc * 10 + D }, number(Acc1, N).
number(N, N) --> [].

digit(D) --> [C], { char_type(C, digit), atom_number(C, D) }.
