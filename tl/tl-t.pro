% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test programs for temporal logic semantic tableaux.

user:file_search_path(common,'../common').
:- ensure_loaded(common(ops)).
:- ensure_loaded(common(io)).
:- ensure_loaded(common(intext)).
:- ensure_loaded(tl).

%  Separate test of SCC algorithm.

test_scc :-
  component_graph(
   [tau(a,b), tau(b,c), tau(b,d), tau(c,a),
    tau(d,d), tau(d,e),
    tau(e,f), tau(f,g), tau(g,h), tau(h,f), tau(h,e)],
    SCCs, Edges),
  write(SCCs), nl,
  write(Edges), nl.

test_tableau(Fml) :-
  to_internal(Fml, FmlI),
  create_tableau(FmlI, Tab, States, Tau, Tab_Result,
                 SCCs, Edges, Fulfil_Result),
  write_tl_tableau(Tab, States, Tau, Tab_Result),
  write_fulfil(Tab_Result, SCCs, Edges, Fulfil_Result).

t1  :- test_tableau(#(p-->q) --> #p --> #q).
t2  :- test_tableau(~(#(p-->q) --> #p --> #q)).
t3  :- test_tableau(# (<>(p ^ q) ^ <>(~ p ^ q) ^ <>(p ^ ~q) ) ).
          % This test is VERY long....
t4  :- test_tableau(~ (# (<>(p ^ q) ^ <>(~p ^ q) ^ <>(p ^ ~q) ) )).
t5  :- test_tableau(~ (# <> # p --> <> # p) ).
t6  :- test_tableau(~ (<> # p --> # <> # p) ).
t7  :- test_tableau(~ ((#p ^ #q) --> (# (p ^ q)))).
t8  :- test_tableau(~ (# ( (p v #q) ^ (#p v q) ) --> (#p v #q))).
t9  :- test_tableau(~ ((#p v #q) --> # ( (p v #q) ^ (#p v q) ))).
t10 :- test_tableau((p ^ q) ^ @ (p ^ q) ).
t11 :- test_tableau(# <> p v <> # ~p ).
t12 :- test_tableau(~(# <> p v <> # ~p )).
t13 :- test_tableau(~(# <> p --> <> # ~p )).

%
%  Test all (except t3) and write to file tl.txt
%
all :-
  tell('tl.txt'),
  t1, t2, t4, t5, t6, t7, t8, t9, t10, t11, t12,
  told.
