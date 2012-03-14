% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Systematic construction of semantic tableaux for
%     the predicate calculus.

%  create(Fml, Tab) - Tab is the tabelau for the formula Fml.
%
%  t(Fmls, Left, Right)
%     Fmls is a list of formula at the root of this tableau and
%     Left and Right are the subtrees (Right ignored for alpha rule).
%     The tableau is constructed by instantiating the logical
%     variables for the subtrees.

create_tableau(Fml, Tab) :-
  Tab = t([Fml], _, _, []), 
  extend_tableau(Tab).

%  extend_tableau(t(Fmls, Left, Right) - Perform one tableau rule.
%    1. Check for a pair of contradicatory formulas in Fmls (closed).
%    2. Check if Fmls contains only literals (open).
%    3. Perform the rules in order: alpha, beta, gamma, delta.

extend_tableau(t(Fmls, closed, empty, _)) :- 
  check_closed(Fmls), !.
extend_tableau(t(Fmls, open,   empty, _)) :- 
  contains_only_literals(Fmls), !.

extend_tableau(t(Fmls, Left,   empty, C)) :-
  alpha_rule(Fmls, Fmls1), !,
  Left = t(Fmls1, _, _, C),
  extend_tableau(Left).

extend_tableau(t(Fmls, Left,   Right, C)) :-
  beta_rule(Fmls, Fmls1, Fmls2),
  Left  = t(Fmls1, _, _, C),
  Right = t(Fmls2, _, _, C),
  extend_tableau(Left),
  extend_tableau(Right).

extend_tableau(t(Fmls, Left,   empty, C)) :-
  delta_rule(Fmls, Fmls1, Const), !,
  Left = t(Fmls1, _, _, [Const|C]),
  extend_tableau(Left).

extend_tableau(t(Fmls, Left,   empty, C)) :-
  gamma_rule(Fmls, Fmls1, C), !,
  Left = t(Fmls1, _, _, C),
  extend_tableau(Left).

%  check_closed(Fmls)
%    Fmls is closed if it contains contradictory formulas.
%  contains_only_literals(Fmls)
%    Traverse Fmls list checking that each is a literal.
%  literal(Fml)
%    Fml is an atomic formula or a negation of one.
%  atomic_formula(Fml)
%    checkes is Fml is an atomic formula

check_closed(Fmls) :-
  member(F1, Fmls), member(neg F2, Fmls), F1 == F2.

contains_only_literals([]).
contains_only_literals([Fml | Tail]) :-
  literal(Fml),
  contains_only_literals(Tail).

literal(Fml)     :- atomic_formula(Fml).
literal(neg Fml) :- atomic_formula(Fml).

atomic_formula(all(_, _)) :- !, fail.
atomic_formula(ex(_, _))  :- !, fail.
atomic_formula(A)         :-
  A =..  [Opr, _, _],
  symbol_opr(_, Opr), !, fail.
atomic_formula(neg _)     :- !, fail.
atomic_formula(_).

%  alpha_rule(Fmls, Fmls1)
%    Fmls1 is Fmls with an alpha deleted and alpha1, alpha2 added.
%  beta_rule(Fmls, Fmls1, Fmls2)
%    Fmls1 (Fmls2) is Fmls with a beta deleted and beta1 (beta2) added.
%  alpha(A1 opr A2, A1, A2)
%  beta(A1 opr A2, A1, A2)
%    Database of rules for each operator.

alpha_rule(Fmls, [A1, A2 | Fmls1]) :-
  member(A, Fmls),
  alpha(A, A1, A2), !,
  delete(Fmls, A, Fmls1).
alpha_rule(Fmls, [A1 | Fmls1]) :-
  member(A, Fmls),
  A = neg neg A1,
  delete(Fmls, A, Fmls1).
  
alpha(A1 and A2, A1, A2).
alpha(neg (A1 imp A2), A1, neg A2).
alpha(neg (A1 or A2), neg A1, neg A2).
alpha(A1 eqv A2, A1 imp A2, A2 imp A1).
  
beta_rule(Fmls, [B1 | Fmls1], [B2 | Fmls1]) :-
  member(B, Fmls),
  beta(B, B1, B2),
  delete(Fmls, B, Fmls1).

beta(B1 or B2, B1, B2).
beta(B1 imp B2, neg B1, B2).
beta(neg (B1 and B2), neg B1, neg B2).
beta(neg (B1 eqv B2),  neg (B1 imp B2), neg (B2 imp B1)).

%  For a list of formulas Fmls, Fmls4 is the list with
%    some gamma formula replaced by all its instances using
%    constants in C

gamma_rule(Fmls, Fmls4, C) :-
  member(A, Fmls),
  is_gamma(A), !,               % Check if gamma rule
  gamma_all(C, A, AList),       % Apply gamma for all constants C
  delete(Fmls, A, Fmls1),       % Re-order: gamma(c), Fmls, gamma
  append(Fmls1, [A], Fmls2),    %   so that substitutions are taken
  append(AList, Fmls2, Fmls3),  %   and universal fml is taken last
  list_to_set(Fmls3, Fmls4).    % Remove duplicates introduced by gamma

%  Just check if this is a gamma formula

is_gamma(all(_, _)).
is_gamma(neg ex(_, _)).

%  gamma_all(C, A, AList)
%    For gamma formula A, AList is a list of instantiations of A
%    with all the constants in the list C

gamma_all([C | Rest], A, [A1 | AList]) :-
  gamma(A, A1, C),
  gamma_all(Rest, A, AList).
gamma_all([], _, []).

%  gamma(A, A1, C)
%    A is a gamma formula, A1 is the instance when the quantified
%    variable is replaced by C

gamma(all(X, A1), A2, C) :-
  instance(A1, A2, X, C).
gamma(neg ex(X, A1), neg A2, C) :-
  instance(A1, A2, X, C).

%  Apply the rule to a delta formula
%    gensym creates a new constant
%    instance builds the instantiation

delta_rule(Fmls, [A2 | Fmls1], C) :-
  member(A, Fmls),
  delta(A, X, A1), !,
  gensym(a, C),
  instance(A1, A2, X, C),
  delete(Fmls, A, Fmls1).

delta(ex(X, A1), X, A1).
delta(neg all(X, A1), X, neg A1).
