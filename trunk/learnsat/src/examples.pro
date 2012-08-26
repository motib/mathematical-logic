% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- ensure_loaded([negation,display,dpll]).

%  Example from Marques-Silva, Lynce, Malik in the Handbook
%  Figures 4.1, 4.2 with changes to ensure the order of assignments

mlm :-
  dpll(
  [
  [x1, x031, ~x2], [x1, ~x3], [x2, x3, x4],
  [~x4, ~x5], [x021, ~x4, ~x6], [x5, x6]
  ], _).

%  Example from Malik-Zhang paper in CACM 52(8), 2009.
%  To force choices, some atoms are renamed and complemented

mz :-
  dpll(
  [
  [ax1, x4], [ax1, bx3, ~x8], [ax1, x8, x12], [cx2, x11],
  [dx7, bx3, x9], [dx7, x8, ~x9], [dx7, x8, ~x10],
  [dx7, x10, ~x12]
  ], _).

%  Example from Marques-Silva and Sakallah GRASP paper

ms :-
  dpll(
  [
  [x1, x2], [x1, x3, ax9], [~x2, ~x3, x4], [~x4, x5, bx10],
  [~x4, x6, cx11], [~x5, ~x6], [~x1, x7, dx12], [~x1, x8],
  [~x7, ~x8, dx13]
  ], _).
