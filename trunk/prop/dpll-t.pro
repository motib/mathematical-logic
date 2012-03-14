% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test four-queens program for dpll

user:file_search_path(common, '../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(dpll).


%  Run four-queens and save the output to a file

four-queens-file :-
  tell('four-queens.txt'),
  four-queens,
  told.

%  Call dpll with the clauses for the four-queens problem  

four-queens :-
  dpll(
  [
  [p11, p12, p13, p14], 
  [p21, p22, p23, p24],
  [p31, p32, p33, p34],
  [p41, p42, p43, p44],
  
  [neg p11, neg p12], [neg p11, neg p13], [neg p11, neg p14], 
  [neg p12, neg p13], [neg p12, neg p14], [neg p13, neg p14],
  [neg p21, neg p22], [neg p21, neg p23], [neg p21, neg p24],
  [neg p22, neg p23], [neg p22, neg p24], [neg p23, neg p24],
  [neg p31, neg p32], [neg p31, neg p33], [neg p31, neg p34],
  [neg p32, neg p33], [neg p32, neg p34], [neg p33, neg p34],
  [neg p41, neg p42], [neg p41, neg p43], [neg p41, neg p44],
  [neg p42, neg p43], [neg p42, neg p44], [neg p43, neg p44],
  
  [neg p11, neg p21], [neg p11, neg p31], [neg p11, neg p41],
  [neg p21, neg p31], [neg p21, neg p41], [neg p31, neg p41],
  [neg p12, neg p22], [neg p12, neg p32], [neg p12, neg p42],
  [neg p22, neg p32], [neg p22, neg p42], [neg p32, neg p42],
  [neg p13, neg p23], [neg p13, neg p33], [neg p13, neg p43],
  [neg p23, neg p33], [neg p23, neg p43], [neg p33, neg p43],
  [neg p14, neg p24], [neg p14, neg p34], [neg p14, neg p44],
  [neg p24, neg p34], [neg p24, neg p44], [neg p34, neg p44],
  
  [neg p11, neg p22], [neg p11, neg p33], [neg p11, neg p44],
  [neg p12, neg p21], [neg p12, neg p23], [neg p12, neg p34],  
  [neg p13, neg p22], [neg p13, neg p31], [neg p13, neg p24],
  [neg p14, neg p23], [neg p14, neg p32], [neg p14, neg p41],
  [neg p21, neg p32], [neg p21, neg p43],
  [neg p22, neg p31], [neg p22, neg p33], [neg p22, neg p44],
  [neg p23, neg p32], [neg p23, neg p41], [neg p23, neg p34],
  [neg p24, neg p33], [neg p24, neg p42],
  [neg p31, neg p42],
  [neg p32, neg p41], [neg p32, neg p43],
  [neg p33, neg p42], [neg p33, neg p44],
  [neg p34, neg p43]
  ]).
