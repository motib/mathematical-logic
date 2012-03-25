% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for proof checker.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(check).

test(List) :-
  transform(List, List1),
  proof(List1).

t1 :-
  test([
    deduce([], (p --> ((p --> p) --> p)) --> ((p --> (p --> p)) --> (p --> p))),
    deduce([],p --> ((p --> p) --> p)),
    deduce([],(p --> (p --> p)) --> (p --> p)),
    deduce([],p --> (p --> p)),
    deduce([],p-->p)
  ]).

t2 :- 
  test([
    deduce([p-->q, q-->r, p], p),
    deduce([p-->q, q-->r, p], p-->q),
    deduce([p-->q, q-->r, p], q),
    deduce([p-->q, q-->r, p], q-->r),
    deduce([p-->q, q-->r, p], r),
    deduce([p-->q, q-->r],    p-->r),
    deduce([p-->q],          (q-->r)-->(p-->r)),
    deduce([],              (p-->q)-->((q-->r)-->(p-->r)))
  ]).

t3 :- 
  test([
    deduce([~p, p], ~p --> (~q --> ~p)),
    deduce([~p, p], ~p),
    deduce([~p, p], ~q --> ~p),
    deduce([~p, p], (~q --> ~p)-->(p-->q)),
    deduce([~p, p], p-->q),
    deduce([~p, p], p),
    deduce([~p, p], q),
    deduce([~p],    p-->q),
    deduce([],      ~p --> (p-->q))
  ]).
