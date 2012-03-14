% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for truth tables.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(def)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(tt).

t1  :- create_tt(p --> r).
t2  :- create_tt(p ^ r).
t3  :- create_tt(p v r).
t4  :- create_tt(p <-> r).
t5  :- create_tt(p ^ (q v r)).
t6  :- create_tt(p <-> p).
t7  :- create_tt(p --> q --> p).
t8  :- create_tt(p --> (q --> r) --> (p --> q) --> (p --> r)).
t9  :- create_tt(p + r).
t10 :- create_tt( (~ p --> ~ q) --> (q --> p) ).
t11 :- create_tt( (p+q) <-> (~ (p -->  q) v ~ (q --> p) )).

