% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test programs for semantic tableaux.


user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).
  :- ensure_loaded(tabl).

test_tableau(Fml) :-
  to_internal(Fml, FmlI),
  create_tableau(FmlI, Tab),
  write_tableau(Tab).

t1 :- test_tableau(
  ~ ( (p --> q --> r ) --> (p --> q) --> (p --> r) ) ).

t2 :- test_tableau(
     (p --> q) --> (r v p ) --> (r v q) ).

t3 :- test_tableau(
  p ^
  (p --> (q v r) ^ ~ (q ^ r)) ^
  (p --> (s v t) ^ ~ (s ^ t)) ^
  (s --> q) ^
  (~ r --> t) ^ 
  (t --> s)
      ).

t4 :- test_tableau(
   ~ (p v q) ^ ~ ~ (p v q)
   ).
