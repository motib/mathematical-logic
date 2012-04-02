% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  module io

%  IO predicates for DPLL

:- module(io, [
  explain/2, explain/3, set_verbose/1,
  write_clause/1, write_clauses/1, write_literal/1
  ]).

:- dynamic verbose/1.

%  Set/get verbose level
%  Default level is 0

set_verbose(N) :-
  (retract(verbose(_)) -> true ; true),
  assert(verbose(N)).

get_verbose(N) :- verbose(N), !.
get_verbose(0).

%  Verbose mode: explanations can be written
%      if verbose level is >= V
%    explain(V, S) - write the atom S
%    explain(V, S, G) - write the atom S and run the goal G

explain(V, S) :-
  get_verbose(N), N >= V, !,
  write(S), nl.
explain(_, _).

explain(V, S, Goal) :-
  get_verbose(N), N >= V, !,
  write(S),
  call(Goal),
  nl.
explain(_, _, _).

%  write_clauses(S) - write a set of clauses
%  write_clause(C)  - write a single clause
%  write_literal(L) - write a single literal

write_clauses([]) :- !,
  write('[]').
write_clauses(A) :-
  write('[\n'),
  write_clauses1(A),
  write(']').

write_clauses1([H]) :- !,
  write_clause(H), nl.
write_clauses1([H|T]) :-
  write_clause(H),
  write(',\n'),
  write_clauses1(T).

write_clause(C) :-
  write('['),
  write_clause1(C),
  write(']').

write_clause1([]) :- !.
write_clause1([H]) :- !,
  write_literal(H).
write_clause1([H|T]) :-
  write_literal(H),
  write(','),
  write_clause1(T).

write_literal(~V) :- !,
  write('~'),
  write(V).
write_literal(V)  :-
  write(V).
