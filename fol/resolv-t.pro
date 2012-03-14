% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for resolution.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(cnffol).
  :- ensure_loaded(skolem).
  :- ensure_loaded(unify).
  :- ensure_loaded(resolv).

test(A) :-
  write_formula(A), nl,
  to_internal(A, AE),
  skolem(AE, B),
  to_external(B, BE),
  write_formula(BE), nl,
  skolem_to_clausal(B, B1),
  write('Set of clauses '),
  write_clauses(B1), nl,
  resolve(B1).

t1 :- test( ~(ex(X, all(Y, p(X, Y))) --> all(Y, ex(X, p(X, Y))))).

t2 :- test( ~(all(X, (p(X) --> q(X))) --> (all(X, p(X)) --> all(X, q(X))))).

t3 :- test( all(X, all(Y, (p(X) v ~p(Y) v q(Y))   ))).

t4 :- test( all(X, all(Y, (p(X) v ~q(Y)) ^ (~p(X) v q(Y))   ))).

t5 :- test( all(X, all(Y, (p(X) v ~q(Y)) ^ ~p(X) ^ (~ q(X))    ))).
