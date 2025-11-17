:- initialization(main, main).

main :-
    read(N),
    read(M),
    read_edges(M, Edges),
    build_graph(N, Edges, Graph, InDegree),
    topological_sort(N, Graph, InDegree, Result),
    (Result = [] ->
        writeln(-1)
    ;
        print_result(Result)
    ),
    halt.

read_edges(0, []) :- !.
read_edges(M, [U-V|Rest]) :-
    M > 0,
    read(U),
    read(V),
    M1 is M - 1,
    read_edges(M1, Rest).

build_graph(N, Edges, Graph, InDegree) :-
    build_empty_graph(N, Graph),
    build_indegree(N, InDegree),
    add_edges(Edges, Graph, InDegree).

build_empty_graph(N, Graph) :-
    functor(Graph, graph, N),
    init_adjacency(N, Graph).

init_adjacency(0, _) :- !.
init_adjacency(I, Graph) :-
    I > 0,
    arg(I, Graph, []),
    I1 is I - 1,
    init_adjacency(I1, Graph).

build_indegree(N, InDegree) :-
    functor(InDegree, indegree, N),
    init_indegree(N, InDegree).

init_indegree(0, _) :- !.
init_indegree(I, InDegree) :-
    I > 0,
    arg(I, InDegree, 0),
    I1 is I - 1,
    init_indegree(I1, InDegree).

add_edges([], _, _).
add_edges([U-V|Rest], Graph, InDegree) :-
    arg(U, Graph, AdjU),
    (member(V, AdjU) -> true ; arg(U, Graph, [V|AdjU])),
    arg(V, InDegree, DegV),
    DegV1 is DegV + 1,
    nb_setarg(V, InDegree, DegV1),
    add_edges(Rest, Graph, InDegree).

topological_sort(N, Graph, InDegree, Result) :-
    find_zero_indegree(N, InDegree, Queue),
    kahn_sort(Queue, Graph, InDegree, [], Result, 0, N).

find_zero_indegree(N, InDegree, Queue) :-
    findall(I, (between(1, N, I), arg(I, InDegree, 0)), Queue).

kahn_sort([], _, _, Acc, Result, Count, N) :-
    (Count =:= N ->
        reverse(Acc, Result)
    ;
        Result = []
    ).
kahn_sort([U|Queue], Graph, InDegree, Acc, Result, Count, N) :-
    Count1 is Count + 1,
    arg(U, Graph, Neighbors),
    process_neighbors(Neighbors, InDegree, NewQueue),
    append(Queue, NewQueue, NextQueue),
    kahn_sort(NextQueue, Graph, InDegree, [U|Acc], Result, Count1, N).

process_neighbors([], _, []).
process_neighbors([V|Rest], InDegree, Queue) :-
    arg(V, InDegree, DegV),
    DegV1 is DegV - 1,
    nb_setarg(V, InDegree, DegV1),
    (DegV1 =:= 0 ->
        Queue = [V|RestQueue]
    ;
        Queue = RestQueue
    ),
    process_neighbors(Rest, InDegree, RestQueue).

print_result([]) :- nl.
print_result([H|T]) :-
    write(H),
    (T = [] -> nl ; (write(' '), print_result(T))).
