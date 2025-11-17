main :-
  read_line_to_codes(user_input, Cs), number_codes(N, Cs),
  S is N*(N+1)//2,
  format('~d~n', [S]).
