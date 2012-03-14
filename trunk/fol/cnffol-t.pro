% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for cnf.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(cnffol).
  
test(A) :-
  write_formula(A), nl,
  to_internal(A, AI),
  cnf(AI, A1),
  to_external(A1, A1E),
  write_formula(A1E), nl,
  cnf_to_clausal(A1, A2),
  write_clauses(A2).

t1  :- test(all(X, (p(X) --> q(X))) --> (all(X, p(X)) --> all(X, q(X)))).
t2  :- test(ex(X, all(Y, p(X, Y))) --> all(Y, ex(X, p(X, Y)))).
t3  :- test(all(X, p(X)) v all(X, q(X))).
t4  :- test(all(X, all(Y, all(Z, p(X,Y) ^ p(Y,Z) --> p(X,Z))))).
t5  :- test(all(X, all(Y, p(X,Y) --> p(Y,X))) ^
      all(X, all(Y, all(Z, p(X,Y) ^ p(Y,Z) --> p(X,Z)))) ^
      all(X, ex(Y, p(X,Y))) ^
      ~ all(X, p(X,X))).
t6  :- test( (p v all(X,q(X))) ^ (r v ex(Y,s(Y))) ).
t7  :- test(all(X, p(X) --> ex(Y, q(Y)))).