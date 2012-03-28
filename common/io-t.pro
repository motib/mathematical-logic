% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test program for writing LaTeX formulas

user:file_search_path(common,'../common').
  :- ensure_loaded(common(ops)).
  :- ensure_loaded(common(intext)).
  :- ensure_loaded(common(io)).

testall :-
  tell('test.tex'),
  write('\\documentclass{article}\n'),
  write('\\usepackage{wasysym}\n'),
  write('\\begin{document}\n'),
  write('\\begin{displaymath}\n'),
  write('\\begin{array}{l}\n'),
  p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11,
  f1, f2, f3, f4,
  t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13,
  write('\\end{array}\n'),
  write('\\end{displaymath}\n'),
  write('\\end{document}\n'), nl,
  told.

test(F) :-
  to_internal(F, F1),
  write_latex(F1),
  write('\\\\\n').
  
test1(F) :-
  to_internal(F, F1),
  write_latex(F1).

p1  :- test(p --> r).
p2  :- test(p ^ r).
p3  :- test(p v r).
p4  :- test(p <-> r).
p5  :- test(p ^ (q v r)).
p6  :- test(p <-> p).
p7  :- test(p --> q --> p).
p8  :- test(p --> (q --> r) --> (p --> q) --> (p --> r)).
p9  :- test(p + r).
p10 :- test( (~ p --> ~ q) --> (q --> p) ).
p11 :- test( (p+q) <-> (~ (p -->  q) v ~ (q --> p) )).

f1 :- test(~ ( all(X, (p(X) --> q(X))) --> (all(X, p(X)) --> all(X, q(X))) )).
f2 :- test(~ (all(X, p(X)) --> all(Y, p(Y)))).
f3 :- test(~ ( ex(X, all(Y, p(X, Y))) --> all(Y, ex(X, p(X, Y))))).
f4 :- test(~ (all(X, p(X) --> q) <-> (ex(X, p(X)) --> q))).

t1  :- test(#(p-->q) --> #p --> #q).
t2  :- test(~(#(p-->q) --> #p --> #q)).
t3  :- test(# (<>(p ^ q) ^ <>(~ p ^ q) ^ <>(p ^ ~q) ) ).
t4  :- test(~ (# (<>(p ^ q) ^ <>(~p ^ q) ^ <>(p ^ ~q) ) )).
t5  :- test(~ (# <> # p --> <> # p) ).
t6  :- test(~ (<> # p --> # <> # p) ).
t7  :- test(~ ((#p ^ #q) --> (# (p ^ q)))).
t8  :- test(~ (# ( (p v #q) ^ (#p v q) ) --> (#p v #q))).
t9  :- test(~ ((#p v #q) --> # ( (p v #q) ^ (#p v q) ))).
t10 :- test((p ^ q) ^ @ (p ^ q) ).
t11 :- test(# <> p v <> # ~p ).
t12 :- test(~(# <> p v <> # ~p )).
t13 :- test(~(# <> p --> <> # ~p )).
