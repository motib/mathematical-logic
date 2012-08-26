% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- ensure_loaded([negation,display,dpll]).

%  3-hole pigeon hole problem
%  pij means that pigeon i is placed in hole j

hole2_file :-
  tell('hole2.txt'),
  hole2,
  told.

hole2 :-
  dpll(
  [
  % Each pigeon in at least hole
  [p11, p12], [p21, p22], [p31, p32], 
  % Each hole has at most one pigeon
  [~p11, ~p21], [~p11, ~p31], [~p21, ~p31], 
  [~p12, ~p22], [~p12, ~p32], [~p22, ~p32]
  ], _).

hole3_file :-
  tell('hole3.txt'),
  hole3,
  told.

hole3 :-
  dpll(
  [
  % Each pigeon in at least hole
  [p11, p12, p13], [p21, p22, p23],
  [p31, p32, p33], [p41, p42, p43], 
  % Each hole has at most one pigeon
  [~p11, ~p21], [~p11, ~p31], [~p11, ~p41],
  [~p21, ~p31], [~p21, ~p41], [~p31, ~p41],
  [~p12, ~p22], [~p12, ~p32], [~p12, ~p42],
  [~p22, ~p32], [~p22, ~p42], [~p32, ~p42],
  [~p13, ~p23], [~p13, ~p33], [~p13, ~p43],
  [~p23, ~p33], [~p23, ~p43], [~p33, ~p43]
  ], _).
