% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for unify.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- use_module(unify).

test(A, B) :-
  unify(A, B, Subst),
  write_unified(A, B, Subst).

t1 :- test(p(g(Y), f(X,h(X),Y)), p(X,f(g(Z),W,Z))).

t2 :- test(p(g(Y), f(X,h(X),Y)), p(X,ff(g(Z),W,Z))).

t3 :- test(p(f(Y,Z,g(Y,Z,X))), p(X)).

t4 :- test(p(f(a,Z),g(X)), p(f(a,Y), g(g(Z)))).

