% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.
%
%  Test program for semantic tableaux.
%

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(def)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(instance).
  :- ensure_loaded(tabl).

test(Fml) :-
  to_internal(Fml, FmlE),
  create_tableau(FmlE, Tab),
  write_tableau(Tab).

t1 :- test(~ ( all(X, (p(X) --> q(X))) --> (all(X, p(X)) --> all(X, q(X))) )).

t2 :- test(~ (all(X, p(X)) --> all(Y, p(Y)))).

t3 :- test(~ ( ex(X, all(Y, p(X, Y))) --> all(Y, ex(X, p(X, Y))))).

t4 :- test(~ (all(X, p(X) --> q) <-> (ex(X, p(X)) --> q))).

