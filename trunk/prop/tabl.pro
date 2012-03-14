% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Systematic construction of semantic tableaux for
%     the propositional calculus.
%   1. Close a branch even if not atomically closed.
%   2. Apply all possible alpha rules before beta rules.

%  create(Fml, Tab) - Tab is the tabelau for the formula Fml.
%
%  t(Fmls, Left, Right)
%     Fmls is a list of formula at the root of this tableau and
%     Left and Right are the subtrees (Right ignored for alpha rule).
%     The tableau is constructed by instantiating the logical
%     variables for the subtrees.

create_tableau(Fml, Tab) :-
  Tab = t([Fml], _, _), 
  extend_tableau(Tab).

%  extend_tableau(t(Fmls, Left, Right) - Perform one tableau rule.
%    1. Check for a pair of contradicatory formulas in Fmls (closed).
%    2. Check if Fmls contains only literals (open).
%    3. Perform an alpha or beta rule:
%         check first for alpha rules that apply anywhere in the list,
%         and only then look for beta rules.

extend_tableau(t(Fmls, closed, empty)) :-
  check_closed(Fmls), !.
extend_tableau(t(Fmls, open,   empty)) :- 
  contains_only_literals(Fmls), !.
extend_tableau(t(Fmls, Left,   empty)) :-
  alpha_rule(Fmls, Fmls1), !,
  Left = t(Fmls1, _, _),
  extend_tableau(Left).
extend_tableau(t(Fmls, Left,   Right)) :-
  beta_rule(Fmls, Fmls1, Fmls2),
  Left  = t(Fmls1, _, _),
  Right = t(Fmls2, _, _),
  extend_tableau(Left),
  extend_tableau(Right).

%  check_closed(Fmls)
%    Fmls is closed if it contains contradictory formulas.
%  contains_only_literals(Fmls)
%    Traverse Fmls list checking that each is a literal.
%  literal(Fml)
%    Fml is a propositional letter or a negation of one.

check_closed(Fmls) :-
  member(F, Fmls), member(neg F, Fmls).

contains_only_literals([]).
contains_only_literals([Fml | Tail]) :-
  literal(Fml),
  contains_only_literals(Tail).

literal(Fml)  :- atom(Fml).
literal(neg Fml) :- atom(Fml).

%  alpha_rule(Fmls, Fmls1)
%    Fmls1 is Fmls with an alpha deleted and alpha1, alpha2 added.
%  beta_rule(Fmls, Fmls1, Fmls2)
%    Fmls1 (Fmls2) is Fmls with a beta deleted and beta1 (beta2) added.

alpha_rule(Fmls, [A1, A2 | Fmls1]) :-
  member(A, Fmls),
  alpha(A, A1, A2), !,
  delete(Fmls, A, Fmls1).
alpha_rule(Fmls, [A1 | Fmls1]) :-
  member(A, Fmls),
  A = neg neg A1,
  delete(Fmls, A, Fmls1).
  
beta_rule(Fmls, [B1 | Fmls1], [B2 | Fmls1]) :-
  member(B, Fmls),
  beta(B, B1, B2),
  delete(Fmls, B, Fmls1).

%  alpha(A1 opr A2, A1, A2)
%  beta(A1 opr A2, A1, A2)
%    Database of rules for each operator.

alpha(A1 and A2, A1, A2).
alpha(neg (A1 imp A2), A1, neg A2).
alpha(neg (A1 or A2), neg A1, neg A2).
alpha(A1 eqv A2, A1 imp A2, A2 imp A1).
  
beta(B1 or B2, B1, B2).
beta(B1 imp B2, neg B1, B2).
beta(neg (B1 and B2), neg B1, neg B2).
beta(neg (B1 eqv B2),  neg (B1 imp B2), neg (B2 imp B1)).
