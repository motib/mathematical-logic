% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test Tseitin clauses for K3,3 using dpll

user:file_search_path(common, '../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(dpll).

%  Run and save the output to a file

k33-file :-
  tell('k33.txt'),
  k33,
  told.

%  Call dpll with the clauses  

k33 :-
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
  ]).
