% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Convert CNF (list of clauses, each clause a list of literals)
%    into DIMACS format

%  to_dimacs(File, Comment, Clauses)
%    File name to write DIMACS format
%    Comment to appear in comments in file
%    Clauses

%  Get number of clauses and variables for the header
%  Then write the clauses

to_dimacs(File, Comment, Clauses) :-
  tell(File),
  length(Clauses, Number_of_Clauses),
  retractall(_),
  count_variables(Clauses, Variables),
  length(Variables, Number_of_Variables),
  write('c\nc '), write(Comment), write('\nc\np  cnf  '),
  write(Number_of_Variables), write('  '),
  write(Number_of_Clauses), nl,
  convert_clauses(Clauses),
  told.

%  count_variables(Clauses, Variables)
%    Return the set of variables (atomic propositions) in the Clauses
%    Traverse the clauses and assert each atom
%    Call setof to obtain a list with no duplicates

count_variables([], Variables) :-
  setof(X, variable(X), Variables),
  retractall(_).
count_variables([Head | Tail], Variables) :-
  count_variables1(Head),
  count_variables(Tail, Variables).

%  count_variables1(Clauses)
%    Assert all the atomic propositions of a single clause

count_variables1([]).
count_variables1([neg A | Tail]) :- !, 
  assert(variable(A)),
  count_variables1(Tail).
count_variables1([A | Tail]) :-
  assert(variable(A)),
  count_variables1(Tail).

% The DIMACS format expects atomic propositions to be number
%   so strip off any non-digits and write the atomic proposition
%   Use '-' for negation and '0' to terminate a clause

convert_clauses([]).
convert_clauses([Head | Tail]) :- 
  convert_clauses1(Head),
  convert_clauses(Tail).

convert_clauses1([]) :-
  write('0'), nl.
convert_clauses1([neg Head | Tail]) :- !,
  write('-'),
  convert_atom(Head),
  convert_clauses1(Tail).
convert_clauses1([Head | Tail]) :-
  convert_atom(Head),
  convert_clauses1(Tail).

%  Convert an atomic proposition to a character list and then
%    remove non-digits from the character list

convert_atom(Atom) :-
  atom_chars(Atom, Chars),
  only_digits(Chars, Chars1),
  atom_chars(Atom1, Chars1),
  write(Atom1), write(' ').

only_digits([], []).
only_digits([Head | Tail], [Head | Tail1]) :- 
  char_type(Head, digit), !,
  only_digits(Tail, Tail1).
only_digits([_ | Tail], Tail1) :- 
  only_digits(Tail, Tail1).
