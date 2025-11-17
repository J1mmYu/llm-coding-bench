:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

main :-
    read_number(N),
    read_list(N, List),
    lis_length(List, Length),
    writeln(Length),
    halt.

read_number(N) :-
    read_line_to_string(user_input, Line),
    number_string(N, Line).

read_list(N, List) :-
    read_line_to_string(user_input, Line),
    split_string(Line, " ", " ", Strings),
    length(Strings, N),
    maplist(number_string, List, Strings).

lis_length(List, Length) :-
    lis_dp(List, Length).

lis_dp(List, MaxLen) :-
    length(List, N),
    (N = 0 -> MaxLen = 0 ;
     lis_dp_helper(List, 1, N, [], MaxLen)).

lis_dp_helper(List, Idx, N, DP, MaxLen) :-
    (Idx > N ->
        max_list(DP, MaxLen)
    ;
        nth1(Idx, List, Current),
        findall(Len,
            (between(1, Idx, J),
             J < Idx,
             nth1(J, List, Prev),
             nth1(J, DP, PrevLen),
             Prev < Current,
             Len = PrevLen),
            Lens),
        (Lens = [] -> CurrentLen = 1 ; max_list(Lens, MaxPrev), CurrentLen is MaxPrev + 1),
        append(DP, [CurrentLen], NewDP),
        NextIdx is Idx + 1,
        lis_dp_helper(List, NextIdx, N, NewDP, MaxLen)
    ).
