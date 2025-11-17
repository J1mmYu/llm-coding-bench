:- set_prolog_flag(verbose, silent).
:- initialization(main, main).

factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.

find_k(N, K) :-
    between(1, 12, K),
    factorial(K, F),
    F mod N =:= 0,
    !.
find_k(_, -1).

main([]) :-
    read(N),
    find_k(N, K),
    writeln(K),
    halt.
