% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  instance/4 is used both in the tableau and proof checker.

:- module(instance, [instance/4]).

%  instance(A, A1, X, C)
%    A1 is an instance of A obtained by substituting C for X.
%    If C is an atom, create instance with C.
%    If C is a variable, check instance and return C.
%  subst_constant(X, C, Vars, Vars1)
%    Vars is a list of variables. Vars1 is Vars with X replaced by C.

instance(all(X1, A), all(X2, A1), Y, C) :-
  var(C), !,
  X1 == X2,
  instance(A, A1, Y, C).
instance(all(X, A), all(X, A1), Y, C) :-
  atom(C), !,
  instance(A, A1, Y, C).
instance(ex(X1, A),  ex(X2, A1), Y, C)  :-
  var(C), !,
  X1 == X2,
  instance(A, A1, Y, C).
instance(ex(X, A),  ex(X, A1), Y, C)  :-
  atom(C), !,
  instance(A, A1, Y, C).
instance(A, A1, X, C) :-
  A =..  [Opr, F1, F2],
  symbol_opr(_, Opr), !,
  A1 =.. [Opr, F1I, F2I],
  instance(F1, F1I, X, C),
  instance(F2, F2I, X, C).
instance(neg A, neg A1, X, C) :- !,
  instance(A, A1, X, C).
instance(A, A1, X, C) :-
  A =..  [F | Vars],
  nonvar(A1),
  A1 =.. [F | Vars1],
  subst_constant(X, C, Vars, Vars1).
instance(A, A1, X, C) :-
  A =..  [F | Vars],
  var(A1),
  subst_constant(X, C, Vars, Vars1),
  A1 =.. [F | Vars1].

subst_constant(X1, C, [X2 | Tail], [C | Tail1]) :-
  X1 == X2, !,
  subst_constant(X1, C, Tail, Tail1).
subst_constant(X, C, [Y1 | Tail], [Y2 | Tail1]) :-
  var(C), Y1 == Y2, !,
  subst_constant(X, C, Tail, Tail1).
subst_constant(X, C, [Y | Tail], [Y | Tail1]) :-
  atom(C), !, subst_constant(X, C, Tail, Tail1).
subst_constant(_, _, [], []).
