% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for resolution.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(resolv).
  :- ensure_loaded(cnfprop).

test(A) :-
  write_formula(A), nl,
  to_internal(A, AI),
  cnf(AI, A1),
  to_external(A1, A1E),
  write_formula(A1E), nl,
  cnf_to_clausal(A1, A2),
  write_clauses(A2), nl, nl, !,
  resolve(A2).

t1 :- test( ~ ( (p --> q --> r ) --> (p --> q) --> (p --> r) ) ).
t2 :- test(  ( (p --> q --> r ) --> (p --> q) --> (p --> r) ) ).
t3 :- test( ~ (p <-> q) ).
t4 :- test(  ((~p) --> (~q)) --> (p --> q) ).
t5 :- test(  (p --> q) --> (r v p ) --> (r v q) ).
