% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test Tseitin clauses for K2,2, K3,3, example in Section 4.5

user:file_search_path(common, '../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
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
  [neg p, q], [p, neg q],
  [p, r, s], [neg p, neg r, s], [neg p, r, neg s], [p, neg r, neg s],
  [neg s, t], [s, neg t],
  [neg q, r, t], [q, neg r, t], [q, r, neg t], [neg q, neg r, neg t]
  ], Mode, _, _).
  
%  Satisfiable set of clauses

exs(Mode) :-
  dpll(
  [
  [neg p, q], [p, neg q],
  [p, r, s], [neg p, neg r, s], [neg p, r, neg s], [p, neg r, neg s],
  [neg s, t], [s, neg t],
  [neg q, r, t], [q, neg r, t], [q, r, neg t], [neg q, neg r, t]
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
  [neg p0, neg p1], [p0,  p1],
  [p0, neg p2], [neg p0,  p2],
  [p1, neg p3], [neg p1,  p3],
  [p2, neg p3], [neg p2,  p3]
  ], Mode, _, _).

%  Satisfiable set of clauses
  
k22s(Mode) :-
  dpll(
  [
  [neg p0, neg p1], [p0,  p1],
  [p0, neg p2], [neg p0,  p2],
  [p1, neg p3], [neg p1,  p3],
  [p2, neg p3], [neg p2,  neg p3]
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
  [neg p0, neg p1,  p2],    [neg p0,  p1, neg p2],    [p0, neg p1, neg p2],
	[p0,  p1,  p2],           [neg p3,  p4,  p5],       [p3, neg p4,  p5],
	[p3,  p4, neg p5],        [neg p3, neg p4, neg p5], [neg p6,  p7,  p8],
	[p6, neg p7,  p8],        [p6,  p7, neg p8],        [neg p6, neg p7, neg p8],
	[neg p0,  p3,  p6],       [p0, neg p3,  p6],        [p0,  p3, neg p6],
	[neg p0, neg p3, neg p6], [neg p1,  p4,  p7],       [p1, neg p4,  p7],
	[p1,  p4, neg p7],        [neg p1, neg p4, neg p7], [neg p2,  p5,  p8],
	[p2, neg p5,  p8],        [p2,  p5, neg p8],        [neg p2, neg p5, neg p8]
  ], Mode, _, _).

%  Satisfiable set of clauses

k33s(Mode) :-
  dpll(
  [
  [neg p0, neg p1,  p2],    [neg p0,  p1, neg p2],    [p0, neg p1, neg p2],
	[p0,  p1,  p2],           [neg p3,  p4,  p5],       [p3, neg p4,  p5],
	[p3,  p4, neg p5],        [neg p3, neg p4, neg p5], [neg p6,  p7,  p8],
	[p6, neg p7,  p8],        [p6,  p7, neg p8],        [neg p6, neg p7, neg p8],
	[neg p0,  p3,  p6],       [p0, neg p3,  p6],        [p0,  p3, neg p6],
	[neg p0, neg p3, neg p6], [neg p1,  p4,  p7],       [p1, neg p4,  p7],
	[p1,  p4, neg p7],        [neg p1, neg p4, neg p7], [neg p2,  p5,  p8],
	[p2, neg p5,  neg p8],        [p2,  p5, neg p8],        [neg p2, neg p5, neg p8]
  ], Mode, _, _).

