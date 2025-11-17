:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

main :-
    read_string(user_input, _, Input),
    split_string(Input, "\n", "\n", Lines),
    Lines = [FirstLine | RestLines],
    split_string(FirstLine, " ", " ", [NStr, MStr]),
    number_string(N, NStr),
    number_string(M, MStr),
    parse_grid(RestLines, 1, Grid),
    count_components(Grid, N, M, Count),
    writeln(Count),
    halt.

parse_grid([], _, []).
parse_grid([Line | Rest], Row, Grid) :-
    (   Line \= ""
    ->  string_chars(Line, Chars),
        parse_row(Chars, Row, 1, RowCells),
        parse_grid(Rest, Row1, RestGrid),
        Row1 is Row + 1,
        append(RowCells, RestGrid, Grid)
    ;   parse_grid(Rest, Row, Grid)
    ).

parse_row([], _, _, []).
parse_row([Char | Rest], Row, Col, [cell(Row, Col, Char) | RestCells]) :-
    Col1 is Col + 1,
    parse_row(Rest, Row, Col1, RestCells).

count_components(Grid, N, M, Count) :-
    count_components(Grid, N, M, [], Count).

count_components([], _, _, _, 0).
count_components([cell(R, C, '.') | Rest], N, M, Visited, Count) :-
    \+ member((R, C), Visited),
    !,
    bfs([(R, C)], N, M, Visited, NewVisited),
    count_components(Rest, N, M, NewVisited, RestCount),
    Count is RestCount + 1.
count_components([cell(R, C, _) | Rest], N, M, Visited, Count) :-
    (   member((R, C), Visited)
    ->  NewVisited = Visited
    ;   NewVisited = [(R, C) | Visited]
    ),
    count_components(Rest, N, M, NewVisited, Count).

bfs([], _, _, Visited, Visited).
bfs([Pos | Queue], N, M, Visited, FinalVisited) :-
    Pos = (R, C),
    (   member(Pos, Visited)
    ->  bfs(Queue, N, M, Visited, FinalVisited)
    ;   get_neighbors(R, C, N, M, Neighbors),
        append(Queue, Neighbors, NewQueue),
        bfs(NewQueue, N, M, [Pos | Visited], FinalVisited)
    ).

get_neighbors(R, C, N, M, Neighbors) :-
    R1 is R - 1, R2 is R + 1,
    C1 is C - 1, C2 is C + 1,
    findall((NR, NC),
            (   (NR = R1, NC = C, R1 >= 1)
            ;   (NR = R2, NC = C, R2 =< N)
            ;   (NR = R, NC = C1, C1 >= 1)
            ;   (NR = R, NC = C2, C2 =< M)
            ),
            Neighbors).
