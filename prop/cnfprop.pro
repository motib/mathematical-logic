% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Transform a formula in propositional logic to CNF

:- module(cnf,
    [cnf/2,
     cnf_to_clausal/2,
     demorgan/2]).

%  cnf(A, B)
%    B is A in conjunctive normal form.

cnf(A1, A4) :-
  eliminate(A1, A2), 
  demorgan(A2, A3),
  distribute(A3, A4).

%  eliminate(A, B)
%    B is A without eqv, xor and imp.

eliminate(A eqv B,
          (A1 and B1) or ((neg A1) and (neg B1)) ) :- !,
  eliminate(A, A1),
  eliminate(B, B1).
eliminate(A xor B,
          (A1 and (neg B1)) or ((neg A1) and B1) ) :- !,
  eliminate(A, A1),
  eliminate(B, B1).
eliminate(A imp B,
          (neg A1) or B1 )     :- !,
  eliminate(A, A1),
  eliminate(B, B1).
eliminate(A or B,  A1 or B1)   :- !,
  eliminate(A, A1),
  eliminate(B, B1).
eliminate(A and B,  A1 and B1) :- !,
  eliminate(A, A1),
  eliminate(B, B1).
eliminate(neg A, neg A1)       :- !,
  eliminate(A, A1).
eliminate(all(X,A), all(X,A1)) :- !,
  eliminate(A, A1).
eliminate(ex(X,A), ex(X,A1))   :- !,
  eliminate(A, A1).
eliminate(A, A).


%  demorgan(A, B)
%    B is A with negations pushed inwards and
%    reducing double negations.

demorgan(neg (A and B), A1 or B1) :- !,
  demorgan(neg A, A1),
  demorgan(neg B, B1).
demorgan(neg (A or B), A1 and B1) :- !,
  demorgan(neg A, A1),
  demorgan(neg B, B1).
demorgan((neg (neg A)),  A1)      :- !,
  demorgan(A, A1).
demorgan(neg all(X,A), ex(X,A1))  :- !,
  demorgan(neg A, A1).
demorgan(neg ex(X,A), all(X,A1))  :- !,
  demorgan(neg A, A1).

demorgan(neg always A, eventually A1)  :- !,
  demorgan(neg A, A1).
demorgan(neg eventually A, always A1)  :- !,
  demorgan(neg A, A1).

demorgan(all(X,A), all(X,A1))     :- !,
  demorgan(A, A1).
demorgan(ex(X,A),   ex(X,A1))     :- !,
  demorgan(A, A1).
demorgan(A and B,    A1 and B1)   :- !,
  demorgan(A, A1),
  demorgan(B, B1).
demorgan(A or B,    A1 or B1)     :- !,
  demorgan(A, A1),
  demorgan(B, B1).
demorgan(A, A).


%  distribute(A, B)
%    B is A with disjuntion distributed over conjunction.

distribute(all(X,A), all(X,A1)) :- !, distribute(A, A1).
distribute(ex(X,A),  ex(X,A1))  :- !, distribute(A, A1).
distribute(A and B, A1 and B1) :- !,
  distribute(A, A1),
  distribute(B, B1).
distribute(A or B, AB) :- !,
  distribute(A, A1),
  distribute(B, B1),
  distribute(A1, B1, AB).
distribute(A, A).

distribute(A and B, C, D) :- !,
  D = D1 and D2,
  distribute(A, C, D1),
  distribute(B, C, D2).
distribute(C, A and B, D) :- !,
  D = D1 and D2,
  distribute(C, A, D1),
  distribute(C, B, D2).
distribute(A, B, A or B).

%  cnf_to_clausal(A, S)
%    A is a CNF formula, S is the formula in clausal notation:
%    a set of sets
%    It recurses on the conjunction and calls disjunction_to_clausal
%    for each disjunction 

cnf_to_clausal(A, S) :-
  cnf_to_clausal(A, [], S1),
  list_to_set(S1, S2),
  reverse(S2, S).

cnf_to_clausal(A1 and A2, SoFar, S2) :- !,
  cnf_to_clausal(A1, SoFar, S1),
  cnf_to_clausal(A2, S1, S2).
cnf_to_clausal(A, SoFar, [S | SoFar]) :-
  disjunction_to_clausal(A, S).

%  Call disjunction_to_clause on to turn the disjunction into a set
%    and return the empty set if there are clashing literals.

disjunction_to_clausal(A, []) :-
  disjunction_to_clause(A, C),
  member(L1, C),
  member(neg L2, C),
  L1 == L2, !.
disjunction_to_clausal(A, C) :-
  disjunction_to_clause(A, C).

disjunction_to_clause(A1 or A2, S) :- !,
  disjunction_to_clause(A1, S1),
  disjunction_to_clause(A2, S2),
  append(S1, S2, S3),
  list_to_set(S3, S4),
  sort(S4, S).
disjunction_to_clause(A, [A]).
