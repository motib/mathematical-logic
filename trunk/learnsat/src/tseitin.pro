% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- use_module(dpll).

%  Test Tseitin clauses for K2,2, K3,3 and example in Section 4.5

%  Run and save the output to a file

ex_file :-
  tell('tseitin-ex.txt'),
  ex,
  told.
ex_file :-
  told.

%  Call dpll with the clauses  

ex :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, ~t]
	], _).
  
%  Satisfiable set of clauses

exs :-
  dpll(
  [
  [~p, q], [p, ~q],
  [p, r, s], [~p, ~r, s], [~p, r, ~s], [p, ~r, ~s],
  [~s, t], [s, ~t],
  [~q, r, t], [q, ~r, t], [q, r, ~t], [~q, ~r, t]
	], _).
  

%  Run and save the output to a file

k22_file :-
  tell('tseitin-k22.txt'),
  k22,
  told.
k22_file :-
  told.

%  Call dpll with the clauses  

k22 :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  p3]
	], _).

%  Satisfiable set of clauses
  
k22s :-
  dpll(
  [
  [~p0, ~p1], [p0,  p1],
  [p0, ~p2], [~p0,  p2],
  [p1, ~p3], [~p1,  p3],
  [p2, ~p3], [~p2,  ~p3]
	], _).
  
%  Run and save the output to a file

k33_file :-
  tell('tseitin-k33.txt'),
  k33,
  told.
k33_file :-
  told.

%  Call dpll with the clauses  

k33 :-
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
	], _).

%  Satisfiable set of clauses

k33s :-
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
	], _).
