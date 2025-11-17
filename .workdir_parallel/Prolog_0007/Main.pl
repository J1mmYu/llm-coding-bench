:- set_prolog_flag(verbose, silent).
:- prompt(_, '').
:- use_module(library(readutil)).

main :-
    read_line_to_string(user_input, S),
    read_string(user_input, "\n", "\r", _, KStr),
    number_string(K, KStr),
    find_smallest_substring(S, K, Result),
    writeln(Result),
    halt.

find_smallest_substring(S, K, Result) :-
    string_length(S, Len),
    MaxStart is Len - K,
    findall(Sub, (between(0, MaxStart, I), sub_string(S, I, K, _, Sub)), Subs),
    sort(Subs, [Result|_]).

:- main.
