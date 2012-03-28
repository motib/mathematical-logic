% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  DPLL algorithm

:- module(dpll, [dpll/4]).

%  dpll(Clauses, Mode, Choices, Assignment)
%    A set of Clauses is a list of lists
%    The Mode is "int" for interactive"
%    It returns the Choices made by the algorithm as well as the
%      satisfying assignment

%    1. Check for empty set of clauses
%    2. Check if empty clause is in the set
%    3. Unit propagation; if successful, recurse
%    4. Interactive: ask the user for a literal that is assigned true
%    5. Assignment true to a literal
%       If the evaluation fails, try another literal

dpll(Clauses, Mode, Choices, Assignment) :-
  dpll1(Clauses, Mode, [], Choices, [], Assignment).

%  dpll(Clauses, Mode, SoFarChoices, Choices, SoFarAssignment) :-

dpll1([], _, L, L, A, A) :- !,
  write('The empty set of clauses is valid\n'),
  write('Choices: '),     put_clause(L), nl,
  write('Assignment:\n'), put_clause(A), nl,
  filter_positive(A, A1),
  write('Positive assignment: '), put_clause(A1).

dpll1(C, _, _, _, _, _) :-
  member([], C), !,
  write('The set contains the empty clause and is unsatisfiable\n'),
  fail.

dpll1(C, Mode, L, L1, A, A1) :-
  find_unit(C, Literal),
  unit_propagate(C, C1), !,
  dpll1(C1, Mode, L, L1, [Literal | A], A1).

dpll1(C, int, L, L1, A, A1) :- !,
  write('----------------------------------\n'),
  write('Current set of clauses:\n'),
  put_clauses(C), nl,
  write('Enter a literal: '),
  read(Literal),
  write(Literal), nl,
  evaluate(C, Literal, C1),
  dpll1(C1, int, [Literal | L], L1, [Literal | A], A1).

dpll1(C, Mode, L, L1, A, A1) :-
  put_clauses(C), nl,
  member(Clause, C),
  member(Literal, Clause),
  write('Choosing literal: '),
  write(Literal), nl,
  evaluate(C, Literal, C1),
  dpll1(C1, Mode, [Literal | L], L1, [Literal | A], A1).

%  Propagate unit clauses by assigning this literal true

unit_propagate(A1, A2) :-
  find_unit(A1, Literal),
  write('Propagating unit literal: '),
  write(Literal), nl,
  evaluate(A1, Literal, A2).

%  Search for a unit clause

find_unit([], _) :- !, fail.
find_unit([H|_], Element) :-
  length(H, 1), !,
  H = [Element].
find_unit([_|T], Element) :-
  find_unit(T, Element).

%  Evaluate a set of clauses A when Literal is set to true and
%    Return the result in A1
%    Eliminate clauses containing the literal and delete its
%    complement from other clauses

evaluate(_, end_of_file, _) :- !,
  abort.
evaluate(A, Literal, A1) :-
  eliminate(A, Literal, [], A2),
  complement(Literal, Literal1),
  delete_complement(A2, Literal1, [], A1),
  explain('Result of unit propagation:\n', dpll:put_clauses(A1)).

%  Complement a literal

complement(neg Literal, Literal) :- !.
complement(Literal, neg Literal).

%  eliminate(A, Literal, SoFar, A1)
%  Eliminate clauses from the set A that contain Literal
%    Return the result in A1
%    SoFar is used to build the result

eliminate([], _, A, A).
eliminate([H|T], Literal, SoFar, A) :-
  member(Literal, H), !,
  explain('Deleting clause: ', dpll:put_clause(H)),
  eliminate(T, Literal, SoFar, A).
eliminate([H|T], Literal, SoFar, A) :-
  eliminate(T, Literal, [H|SoFar], A).

%  delete_complement(A, Literal, SoFar, A)
%  Delete Literal from the set of clauses A
%    Return the result in A1
%    SoFar is used to build the result
  
delete_complement([], _, A, A).
delete_complement([H|T], Literal, SoFar, A) :-
  member(Literal, H), !,
  explain('Deleting literal from clause: ', dpll:put_clause(H)),
  delete(H, Literal, H1),
  delete_complement(T, Literal, [H1|SoFar], A).
delete_complement([H|T], Literal, SoFar, A) :-
  delete_complement(T, Literal, [H|SoFar], A).

%  filter positive literals only

filter_positive([], []) :- !.
filter_positive([neg _ | Tail], Tail1) :- !,
  filter_positive(Tail, Tail1).
filter_positive([L | Tail], [L | Tail1]) :- !,
  filter_positive(Tail, Tail1).

%  Special version of write_clauses for this algorithm
%  Each clause is on a separate line and "neg" is used instead of "~"

put_clauses([]) :- !,
  write('[]').
put_clauses(A) :-
  write('[\n'),
  put_clauses1(A),
  write(']').

put_clauses1([H]) :- !,
  put_clause(H), nl.
put_clauses1([H|T]) :-
  put_clause(H),
  write(',\n'),
  put_clauses1(T).

put_clause(C) :-
  write('['),
  put_clause1(C),
  write(']').

put_clause1([]) :- !.
put_clause1([H]) :- !,
  write_formula(H).
put_clause1([H|T]) :-
  write_formula(H),
  write(','),
  put_clause1(T).
