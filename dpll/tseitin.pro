% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Test Tseitin clauses for K2,2, K3,3, example in Section 4.5

:- ensure_loaded(neg).
:- ensure_loaded(dpll).

%  Run and save the output to a file

ex_file(Mode) :-
  tell('tseitin-ex.txt'),
  ex(Mode),
  told.
ex_file(_) :-
  told.

%  Call dpll with the clauses  

ex(Mode) :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, ~t]
  ], Mode, _, _).
  
%  Satisfiable set of clauses

exs(Mode) :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, t]
  ], Mode, _, _).
  

%  Run and save the output to a file

k22_file(Mode) :-
  tell('tseitin-k22.txt'),
  k22(Mode),
  told.
k22_file(_) :-
  told.

%  Call dpll with the clauses  

k22(Mode) :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  p3]
  ], Mode, _, _).

%  Satisfiable set of clauses
  
k22s(Mode) :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  ~p3]
  ], Mode, _, _).
  
%  Run and save the output to a file

k33_file(Mode) :-
  tell('tseitin-k33.txt'),
  k33(Mode),
  told.
k33_file(_) :-
  told.

%  Call dpll with the clauses  

k33(Mode) :-
  dpll(
  [
  [~p0, ~p1,  p2],    [~p0,  p1, ~p2],    [p0, ~p1, ~p2],
	[p0,  p1,  p2],           [~p3,  p4,  p5],       [p3, ~p4,  p5],
	[p3,  p4, ~p5],        [~p3, ~p4, ~p5], [~p6,  p7,  p8],
	[p6, ~p7,  p8],        [p6,  p7, ~p8],        [~p6, ~p7, ~p8],
	[~p0,  p3,  p6],       [p0, ~p3,  p6],        [p0,  p3, ~p6],
	[~p0, ~p3, ~p6], [~p1,  p4,  p7],       [p1, ~p4,  p7],
	[p1,  p4, ~p7],        [~p1, ~p4, ~p7], [~p2,  p5,  p8],
	[p2, ~p5,  p8],        [p2,  p5, ~p8],        [~p2, ~p5, ~p8]
  ], Mode, _, _).

%  Satisfiable set of clauses

k33s(Mode) :-
  dpll(
  [
  [~p0, ~p1,  p2],    [~p0,  p1, ~p2],    [p0, ~p1, ~p2],
	[p0,  p1,  p2],           [~p3,  p4,  p5],       [p3, ~p4,  p5],
	[p3,  p4, ~p5],        [~p3, ~p4, ~p5], [~p6,  p7,  p8],
	[p6, ~p7,  p8],        [p6,  p7, ~p8],        [~p6, ~p7, ~p8],
	[~p0,  p3,  p6],       [p0, ~p3,  p6],        [p0,  p3, ~p6],
	[~p0, ~p3, ~p6], [~p1,  p4,  p7],       [p1, ~p4,  p7],
	[p1,  p4, ~p7],        [~p1, ~p4, ~p7], [~p2,  p5,  p8],
	[p2, ~p5,  ~p8],        [p2,  p5, ~p8],        [~p2, ~p5, ~p8]
  ], Mode, _, _).
