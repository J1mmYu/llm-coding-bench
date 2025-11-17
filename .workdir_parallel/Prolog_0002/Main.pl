main :-
  read_line_to_codes(user_input, Cs), reverse(Cs, Rc),
  string_codes(S, Rc), format('~w~n', [S]).
