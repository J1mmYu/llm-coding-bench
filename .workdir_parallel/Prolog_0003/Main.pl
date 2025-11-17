:- use_module(library(readutil)).

read_number(N) :-
    read_line_to_string(user_input, Line),
    number_string(N, Line).

read_numbers(Numbers) :-
    read_line_to_string(user_input, Line),
    split_string(Line, " ", " ", Parts),
    maplist(number_string, Numbers, Parts).

solve :-
    read_number(N),
    read_numbers(Array),
    find_longest_zero_sum(Array, Length),
    writeln(Length).

find_longest_zero_sum(Array, MaxLen) :-
    prefix_sums(Array, 0, [], PrefixList),
    reverse(PrefixList, Prefixes),
    find_max_distance(Prefixes, 0, MaxLen).

prefix_sums([], _, Acc, Acc).
prefix_sums([H|T], CumSum, Acc, Result) :-
    NewSum is CumSum + H,
    prefix_sums(T, NewSum, [NewSum|Acc], Result).

find_max_distance(Prefixes, CurrentMax, MaxLen) :-
    add_indices(Prefixes, 1, IndexedPrefixes),
    IndexedWithZero = [(0, 0)|IndexedPrefixes],
    findall(Dist, (
        member((Sum, I), IndexedWithZero),
        member((Sum, J), IndexedWithZero),
        I < J,
        Dist is J - I
    ), Distances),
    (Distances = [] -> MaxLen = 0 ; max_list(Distances, MaxLen)).

add_indices([], _, []).
add_indices([H|T], I, [(H, I)|Rest]) :-
    I1 is I + 1,
    add_indices(T, I1, Rest).

:- initialization(solve, main).
