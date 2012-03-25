% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  DPLL algorithm

:- module(dpll, [dpll/1]).

%  dpll
%    A set of clauses is a list of lists
%    1. Check for empty set of clauses
%    2. Check if empty clause is in the set
%    3. Unit propagation; if successful, recurse
%    4. Ask the user for a literal that is assigned true
%       If successful, recurse, else repeat to try another literal

dpll([]) :- !,
  put_clauses([]),
  write('The empty set of clauses is valid').

dpll(A) :-
  member([], A), !,
  put_clauses(A),
  write('The set contains the empty clause and is unsatisfiable').

dpll(A) :-
  find_unit(A, _),
  put_clauses(A),
  unit_propagate(A, A1), !,
  dpll(A1).

dpll(A) :-
  put_clauses(A),
  write('Enter a literal: '),
  read(Literal),
  write(Literal), nl,
  evaluate(A, Literal, A1),
  dpll(A1).

%  Propagate unit clauses by assigning this literal true

unit_propagate(A1, A2) :- !,
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
  
evaluate(A, Literal, A1) :-
  eliminate(A, Literal, [], A2),
  complement(Literal, Literal1),
  delete_complement(A2, Literal1, [], A1).

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


%  Special version of write_clauses for this algorithm
%  Each clause is on a separate line and "neg" is used instead of "~"

put_clauses([]) :- !,
  write('[]'), nl.
put_clauses(A) :-
  write('['), nl,
  put_clauses1(A),
  write(']'), nl.

put_clauses1([H]) :- !,
  write('['),
  put_clause(H),
  write(']'), nl.
put_clauses1([H|T]) :-
  put_clause(H),
  write(','), nl,
  put_clauses1(T).

put_clause(C) :-
  write('['),
  put_clause1(C),
  write(']').

put_clause1([]) :- !.
put_clause1([H]) :-
  write_formula(H).
put_clause1([H|T]) :-
  write_formula(H),
  write(','),
  put_clause1(T).
