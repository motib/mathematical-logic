% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for skolemization.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(cnffol).
  :- ensure_loaded(skolem).

test(A) :-
  write_formula(A), nl,
  to_internal(A, AI),
  skolem(AI, B),
  to_external(B, BE),
  write_formula(BE), nl,
  skolem_to_clausal(B, C),
  write_clauses(C), nl.

t1 :- test(all(X, (p(X) --> q(X))) --> (all(X, p(X)) --> all(X, q(X)))).
t2 :- test(ex(X, all(Y, p(X, Y))) --> all(Y, ex(X, p(X, Y)))).
t3 :- test(all(X, all(Y, p(X,Y) --> p(Y,X))) ^
      all(X, all(Y, all(Z, p(X,Y) ^ p(Y,Z) --> p(X,Z)))) ^
      all(X, ex(Y, p(X,Y))) ^
      ~ all(X, p(X,X))).

