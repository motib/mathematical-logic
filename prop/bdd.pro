% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Implementation of binary decision diagrams (BDD)

:- module(bdd,
     [reduce/2,
      apply/4,
      restrict/4,
      exists/3,
      forall/3,
      literal/3]).

%  Cache BDDs for reduce and cache pairs of BDDs for apply.
%
%  bdd(N, False, True)
%    - bdd for variable N with subtrees False, True.
%  bdd(leaf, Value, x)
%    - bdd for terminal with Value t or f (x is dummy).
%
%  bdd_pair(B1, B2, B)
%    - B is the result of applying an operator to B1, B2.

:- dynamic bdd/3, bdd_pair/3.


%  reduce(B1, B2) - B2 is the reduced bdd for B1.
%    - Clear the cache and call reduce1.
%    - Check if node is cached.
%    - Return (and cache) leaf.
%    - Recursively reduce nonterminals

reduce(B1, B2) :-
  retractall(bdd(_,_,_)),
  reduce1(B1, B2).

reduce1(B, B) :- B, !.
reduce1(B, B) :-
  B = bdd(leaf, _, _), !,
  assert(B).
reduce1(bdd(N, False, True), NewNode) :-
  reduce1(False,  NewFalse),
  reduce1(True, NewTrue),
  remove(bdd(N, NewFalse, NewTrue), NewNode).

%  remove(B1, B2) - Remove independent nonterminals.
%    - if left and right subtrees are identical, return one of them,
%      otherwise return (and cache) the node

remove(bdd(_, Subtree, Subtree), Subtree) :- !.
remove(B, B) :- assert(B).


%  apply(B1, Opr, B2, B)  - apply Opr to BDDs: B = B1 Opr B2.
%  
%    - Clear cache and call apply1.
%    - Check for cached pair,
%    -   otherwise call create a new nonterminal,
%          check if it can be reduced and assert it.

apply(B1, Opr, B2, B) :- 
  retractall(bdd_pair(_,_,_)),
  retractall(bdd(_,_,_)),
  apply1(B1, Opr, B2, B).

apply1(B1, _, B2, Result) :-
  bdd_pair(B1, B2, Result), !.
apply1(B1, Opr, B2, Result1) :-
  create(B1, Opr, B2, Result),
  check_reduced(Result, Result1),
  assert(bdd_pair(B1, B2, Result1)).

%  create(B1, Opr, B2, B)
%    - create new nonterminal: B = B1 Opr B2.
%    - create has six clauses:
%        (1)   two leaves, apply operator.
%        (2-3) one leaf, check for controlling operand.
%        (4)   nonterminals for same variable, recurse on subtrees.
%        (5-6) nonterminals for different variables,
%                recurse on the smaller variable.

create(bdd(leaf, Val1, _), Opr,         % Both nodes are leaves
       bdd(leaf, Val2, _),
       bdd(leaf, ValResult, x)) :- !,
  opr(Opr, Val1, Val2, ValResult).

create(bdd(leaf, Val, _), Opr,          % True node is leaf
       _,
       bdd(leaf, Val, x)) :-
  controlling(Opr, Val), !.

create(_, Opr,                          % False node is leaf
       bdd(leaf, Val, _),
       bdd(leaf, Val, x)) :-
  controlling(Opr, Val), !.  

create(bdd(N, False1, True1), Opr,      % Nonterminal for same variable
       bdd(N, False2, True2),
       bdd(N, FalseResult, TrueResult)) :- !,
  apply1(False1, Opr, False2, FalseResult),
  apply1(True1,  Opr, True2,  TrueResult).

create(bdd(N1, False1, True1), Opr,     % True node is leaf or smaller
       Node2,
       bdd(N1, FalseResult, TrueResult)) :-
  Node2 = bdd(N2, _, _),                   
  (N2 = leaf ;
   (N1 \= leaf, N1 < N2)), !,           % Check N1\=leaf before "<"
  apply1(False1,  Opr, Node2, FalseResult),
  apply1(True1, Opr, Node2, TrueResult).

create(Node1, Opr,                      % False node is leaf or smaller
       bdd(N2, False2, True2),
       bdd(N2, FalseResult, TrueResult)) :-
  apply1(Node1, Opr, False2,  FalseResult),
  apply1(Node1, Opr, True2, TrueResult).

%  check_reduced(B1, B2)   - reduce B1 before returning as B2.
%     (1) identical subtrees
%     (2) dode already in database
%     (3) do not need to reduce, so assert into database

check_reduced(bdd(_, Subtree, Subtree), Subtree) :- !.
check_reduced(Result, Result) :- Result, !.
check_reduced(Result, Result) :- assert(Result).

%  controlling(Opr, V)     - V is a controlling operand for Opr.

controlling(or,  t).
controlling(and, f).


%  literal(Sign, N, BDD) -
%    BDD of a literal for variable N with Sign - pos or neg.

literal(pos, N, bdd(N, bdd(leaf, f, x), bdd(leaf, t, x))).
literal(neg, N, bdd(N, bdd(leaf, t, x), bdd(leaf, f, x))).


%  restrict(B1, Variable, Value, B2) -
%    B2 is the restriction of B1 by assigning Value to Variable.
%    (1) a leaf is the restriction of itself.
%    (2-3) for a nonterminal on Variable,
%          return the False or True node depending on Value.
%    (4) fecurse on subBDDs.
%    Reduce all results before returning.

restrict(B1, Var, Val, B3) :-
  retractall(bdd(_,_,_)),
  restrict1(B1, Var, Val, B2),
  check_reduced(B2, B3).

restrict1(bdd(leaf, Val, x), _, _, bdd(leaf, Val, x)) :- !.

restrict1(bdd(N, False,    _), N,  f, False1)  :-
  check_reduced(False, False1), !.
restrict1(bdd(N, _,   True), N,  t, True1) :-
  check_reduced(True, True1), !.

restrict1(bdd(N, False, True), K,  V,
          bdd(N, False1R, True1R)) :-
  restrict1(False,  K, V, False1),
  check_reduced(False1, False1R),
  restrict1(True, K, V, True1),
  check_reduced(True1, True1R).

%  exists(B1, Variable, B2) -
%    B2 is the existential quantification of B1 on Variable.

exists(B1, V, B2) :-
  restrict(B1, V, t, BT),
  restrict(B1, V, f, BF),
  apply(BT, or, BF, B2).

%  forall(B1, Variable, B2) -
%    B2 is the universal quantification of B1 on Variable.

forall(B1, V, B2) :-
  restrict(B1, V, t, BT),
  restrict(B1, V, f, BF),
  apply(BT, and, BF, B2).
