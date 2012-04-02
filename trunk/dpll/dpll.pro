% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  DPLL algorithm

:- module(dpll, [dpll/4]).

:- ensure_loaded(io).

%  dpll(Clauses, Mode, Set, Choices, Assignment)
%    A set of Clauses is a list of lists
%    The Mode is "int" for interactive"
%    Set is the set of literals to be assigned
%      Since it is a _set_ the same literal won't be assigned twice
%    It returns the Choices made by the algorithm as well as the
%      satisfying assignment
%    Write the list of literals and the list of variables in the set
%      before calling dpll1

dpll(Clauses, Mode, Choices, Assignment) :-
  flatten(Clauses, Literals_List),
  list_to_set(Literals_List, Literals_Set),
  explain(2, 'Set of literals: \n', write_clause(Literals_Set)),
  literals_to_variables(Literals_Set, Variables_List),
  list_to_set(Variables_List, Variables_Set),
  explain(2, 'Set of variables: \n', write_clause(Variables_Set)),
  dpll1(Clauses, Mode, Literals_Set, [], Choices, [], Assignment).

%  dpll1(Clauses, Mode, Set, SoFarChoices,
%        Choices, SoFarAssignment, Assignment)
%    1.  Check for empty set of clauses
%    2.  Check if empty clause is in the set
%    3.  Unit propagation; if successful, recurse
%    4a. Interactive mode: ask for a literal that is assigned true
%    4b. Choose a literal that is assigned true
%    5.  Evaluate: recurse if successful, backtrack is not

dpll1([], _, _, L, L, A, A) :- !,
  explain(0, 'The empty set of clauses is valid'),
  explain(1, 'Choices: \n', write_clause(L)),
  explain(1, 'Assignment:\n', write_clause(A)),
  filter_positive(A, A1),
  explain(1, 'Positive assignment: ', write_clause(A1)).

dpll1(C, _, _, _, _, _, _) :-
  member([], C), !,
  explain(0, 'The set contains the empty clause and is unsatisfiable\n'),
  fail.

dpll1(C, Mode, Set, L, L1, A, A1) :-
  find_unit(C, Literal),
  unit_propagate(C, C1), !,
  dpll1(C1, Mode, Set, L, L1, [Literal | A], A1).

dpll1(C, int, Set, L, L1, A, A1) :- !,
  write('----------------------------------\n'),
  write('Current set of clauses:\n'),
  write_clauses(C), nl,
  write('Enter a literal: '),
  read(Literal),
  write(Literal), nl,
  evaluate(C, Literal, C1),
  dpll1(C1, int, Set, [Literal | L], L1, [Literal | A], A1).

dpll1(C, Mode, Set, L, L1, A, A1) :-
  explain(1, 'Current set of clauses:\n', write_clauses(C)),
  member(Literal, Set),
  explain(0, 'Choosing literal: ', write(Literal)),
  evaluate(C, Literal, C1),
  subtract(Set, [Literal], Set1),
  dpll1(C1, Mode, Set1, [Literal | L], L1, [Literal | A], A1).

%  Propagate unit clauses by assigning this literal true

unit_propagate(A1, A2) :-
  find_unit(A1, Literal),
  explain(1, 'Propagating unit literal: ', write(Literal)),
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
%    Eliminate clauses containing the literal and
%      delete its complement from other clauses

evaluate(_, end_of_file, _) :- !,
  abort.
evaluate(A, Literal, A1) :-
  eliminate(A, Literal, [], A2),
  complement(Literal, Literal1),
  delete_complement(A2, Literal1, [], A1),
  explain(2, 'Result of unit propagation:\n', write_clauses(A1)).

%  Complement a literal

complement(~ Literal, Literal) :- !.
complement(Literal, ~ Literal).

%  eliminate(A, Literal, SoFar, A1)
%    Eliminate clauses from the set A that contain Literal
%    Return the result in A1
%    SoFar is used to build the result

eliminate([], _, A, A).
eliminate([H|T], Literal, SoFar, A) :-
  member(Literal, H), !,
  explain(3, 'Deleting clause: ', write_clause(H)),
  eliminate(T, Literal, SoFar, A).
eliminate([H|T], Literal, SoFar, A) :-
  eliminate(T, Literal, [H|SoFar], A).

%  delete_complement(A, Literal, SoFar, A)
%    Delete Literal from the set of clauses A
%    Return the result in A1
%    SoFar is used to build the result
  
delete_complement([], _, A, A).
delete_complement([H|T], Literal, SoFar, A) :-
  member(Literal, H), !,
  explain(3, 'Deleting literal from clause: ', write_clause(H)),
  delete(H, Literal, H1),
  delete_complement(T, Literal, [H1|SoFar], A).
delete_complement([H|T], Literal, SoFar, A) :-
  delete_complement(T, Literal, [H|SoFar], A).

%  filter positive literals only

filter_positive([], []) :- !.
filter_positive([~ _ | Tail], Tail1) :- !,
  filter_positive(Tail, Tail1).
filter_positive([L | Tail], [L | Tail1]) :- !,
  filter_positive(Tail, Tail1).

%  Transform a set of literals into a set of variables

literals_to_variables([], []).
literals_to_variables([~V | Tail], [V | Tail1]) :- !,
  literals_to_variables(Tail, Tail1).
literals_to_variables([V  | Tail], [V | Tail1]) :-
  literals_to_variables(Tail, Tail1).
