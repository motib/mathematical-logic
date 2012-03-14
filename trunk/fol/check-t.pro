% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for proof checker in first-order logic.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(instance).
  :- ensure_loaded(check).

test(List) :-
  transform(List, List1),
  proof(List1).

t1 :-
  test([
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))], all(X, p(X))),
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))], all(X, p(X)) --> p(a)),
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))], p(a)),
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))], all(X, (p(X) --> q(X)))),
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))],
      all(X, (p(X) --> q(X))) --> (p(a) --> q(a))),
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))], p(a) --> q(a)),
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))], q(a)),
    deduce([all(X, (p(X) --> q(X))), all(X, p(X))], all(X, q(X))),
    deduce([all(X, (p(X) --> q(X)))], all(X, p(X)) --> all(X, q(X))),
    deduce([], all(X, (p(X) --> q(X))) --> (all(X, p(X)) --> all(X, q(X))))
   ]).

t2 :-
  test([
    deduce([], all(X, p(X)) --> p(a)),
    deduce([], all(Y, all(X, p(X)) --> p(Y))),
    deduce([], (all(Y, all(X, p(X)) --> p(Y))) -->
                (all(X, p(X)) --> all(Y, p(Y)))),
    deduce([], all(X, p(X)) --> all(Y, p(Y)))
  ]).


t3 :-  % test proviso
  test([
    deduce([all(X, (p(X) --> q(X))), p(a)], p(a)),
    deduce([all(X, (p(X) --> q(X))), p(a)], all(X, (p(X) --> q(X)))),
    deduce([all(X, (p(X) --> q(X))), p(a)], all(X, (p(X) --> q(X))) --> (p(a) --> q(a))),
    deduce([all(X, (p(X) --> q(X))), p(a)], p(a) --> q(a)),
    deduce([all(X, (p(X) --> q(X))), p(a)], q(a)),
    deduce([all(X, (p(X) --> q(X))), p(a)], all(X, q(X))),
    deduce([all(X, (p(X) --> q(X)))], p(a) --> all(X, q(X))),
    deduce([], all(X, (p(X) --> q(X))) --> (p(a) --> all(X, q(X))))
  ]).
