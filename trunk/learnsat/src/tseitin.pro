% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- use_module(dpll).

%  Tseitin clauses for example in Section 4.5 of MLCS:
%  M. Ben-Ari. Mathematical Logic for Computer Science (Third Edition).
%  Springer, 2012.

ex :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, ~t]
	], _).
  
%  Satisfiable variant of the above formula

exs :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, t]
	], _).


%  Tseitin clauses for K_{2,2}  

k22 :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  p3]
	], _).

%  Satisfiable variant of the above formula

k22s :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  ~p3]
	], _).
  
%  Tseitin clauses for K_{3,3}

k33 :-
  dpll(
  [
  [~p0, ~p1,  p2], [~p0,  p1, ~p2], [p0, ~p1, ~p2],
	[p0,  p1,  p2],  [~p3,  p4,  p5], [p3, ~p4,  p5],
	[p3,  p4, ~p5],  [~p3, ~p4, ~p5], [~p6,  p7,  p8],
	[p6, ~p7,  p8],  [p6,  p7, ~p8],  [~p6, ~p7, ~p8],
	[~p0,  p3,  p6], [p0, ~p3,  p6],  [p0,  p3, ~p6],
	[~p0, ~p3, ~p6], [~p1,  p4,  p7], [p1, ~p4,  p7],
	[p1,  p4, ~p7],  [~p1, ~p4, ~p7], [~p2,  p5,  p8],
	[p2, ~p5,  p8],  [p2,  p5, ~p8],  [~p2, ~p5, ~p8]
	], _).

%  Satisfiable variant of the above formula

k33s :-
  dpll(
  [
  [~p0, ~p1,  p2], [~p0,  p1, ~p2], [p0, ~p1, ~p2],
	[p0,  p1,  p2],  [~p3,  p4,  p5], [p3, ~p4,  p5],
	[p3,  p4, ~p5],  [~p3, ~p4, ~p5], [~p6,  p7,  p8],
	[p6, ~p7,  p8],  [p6,  p7, ~p8],  [~p6, ~p7, ~p8],
	[~p0,  p3,  p6], [p0, ~p3,  p6],  [p0,  p3, ~p6],
	[~p0, ~p3, ~p6], [~p1,  p4,  p7], [p1, ~p4,  p7],
	[p1,  p4, ~p7],  [~p1, ~p4, ~p7], [~p2,  p5,  p8],
	[p2, ~p5,  ~p8], [p2,  p5, ~p8],  [~p2, ~p5, ~p8]
	], _).
