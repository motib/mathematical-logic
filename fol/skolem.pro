% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Skolemize a formula.

:- module(skolem, [skolem/2, skolem_to_clausal/2]).

%  skolem_to_clausal(A, B) -
%    A is a skolemized formula, B is A in clausal notation.

skolem_to_clausal(all(_,F), F1) :- !,   % Strip quantifiers
  skolem_to_clausal(F, F1).
skolem_to_clausal(F, F1) :-
  cnf_to_clausal(F, F1).                % Matrix to clausal notation.

%  skolem(A, B)
%  skolem(A, ListA, ListE, B)
%    B is A with existential quantifiers replaced by Skolem functions.
%    ListA of universally quantified variables seen so far.
%    ListE of pairs of existentially quantified variables
%      and the associated Skolem function (X, f(...)).

skolem(A, A2) :- 
   cnf(A, A1),
   skolem(A1, [], [], A2).

%  Universal quantifier: add variable to list seen so far.

skolem(all(X, A), ListA, ListE, all(X, B)) :- !,
  skolem(A, [X | ListA], ListE, B).

% Existential quantifier: get new function symbol and
%   create f(...) from function symbol and universally quantified variables.

skolem(ex(X, A), ListA, ListE, B) :- !,
  gensym(f, F),
  Function =.. [F | ListA],    
  skolem(A, ListA, [(X, Function) | ListE], B).

skolem(A1 or A2, _, ListE, B1 or B2) :- !,
  skolem(A1, _, ListE, B1),
  skolem(A2, _, ListE, B2).
skolem(A1 and A2, _, ListE, B1 and B2) :- !,
  skolem(A1, _, ListE, B1),
  skolem(A2, _, ListE, B2).
skolem(neg A, _, ListE, neg B) :- !,
  skolem(A, _, ListE, B).

% Substitute in atoms for existentially quantified variables.

skolem(A, _, ListE, B) :-
  A =.. [F | Vars],
  subst_var(Vars, Vars1, ListE),
  B =.. [F | Vars1].
