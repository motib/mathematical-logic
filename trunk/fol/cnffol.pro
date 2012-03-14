% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Transform a formula in first-order logic to CNF

:- module(cnf,
    [cnf/2,
     cnf_to_clausal/2,
     demorgan/2,
     subst_var/3]).

%  cnf(A, B)
%    B is A in conjunctive normal form.

cnf(A, A5) :-
  rename(A, A1),
  eliminate(A1, A2), 
  demorgan(A2, A3),
  extract(A3, A4),
  distribute(A4, A5).

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

%  rename(A, B)
%  rename(A, List, Num, B)
%    B is A with bound variables renamed.
%    List is a list of pairs (X, Y),
%      where the Xs are all the variables,
%      and Y is X for the first occurrence of X
%      or a new variable for subsequent occurrences.
%    List1 is List appended with new substitutions created.

rename(A, B) :- rename(A, [], _, B).

% The variable was already encountered, so create a new variable
rename(all(X, A), List, List1, all(Y, A1)) :-
  member_var((X, _), List), !,     
  copy_term(X, Y),
  rename(A, [(X, Y) | List], List1, A1).

% The first occurence of a variable can be replace by itself
rename(all(X, A), List, List1, all(X, A1)) :- !,
  rename(A, [(X, X) | List], List1, A1).

% Similarly for existential quantifiers
rename(ex(X, A), List, List1, ex(Y, A1)) :-
  member_var((X, _), List), !,
  copy_term(X, Y),
  rename(A, [(X, Y) | List], List1, A1).
rename(ex(X, A), List, List1, ex(X, A1)) :- !, 
  rename(A, [(X, X) | List], List1, A1).

% For Boolean operators just recurse
rename(A, List, List2, B) :-
  A =.. [Opr, A1, A2],
  symbol_opr(_, Opr), !,
  rename(A1, List, List1, B1),
  rename(A2, List1, List2, B2),
  B =.. [Opr, B1, B2].
rename(neg A, List, List1, neg B) :- !,
  rename(A, List, List1, B).

%  For atomic formulas, replace the variables
rename(A, List, List, B) :-
  A =.. [F | Vars],
  subst_var(Vars, Vars1, List),
  B =.. [F | Vars1].

%  member_var((X,Y), List)
%    Finds the first pair (X,Y) in List.
%    Called with X bound to a variable and Y not bound.
%    Use == in order not to unify

member_var((A,Y), [(B,Y) | _]) :- A == B, !.
member_var(A, [_ | C]) :- member_var(A, C).

%  subst_var(V1List, V2List, List)
%    V1List is a list of variables, and List is a list of variable pairs
%    For X in V1List, the corresponding element in V2List is Y.
%  The third clause is used in skolem, not in cnf.

subst_var([], [], _).
subst_var([V | Tail], [V1 | Tail1], List) :-
  member_var((V, V1), List), !,
  subst_var(Tail, Tail1, List).
subst_var([V | Tail], [V | Tail1], List) :-
  subst_var(Tail, Tail1, List).

%  extract(A, B)
%    B is A with quantifiers extracted to prefix.

extract(all(X,A), all(X,A1)) :- !, extract(A, A1).
extract(ex(X,A), ex(X,A1)) :- !, extract(A, A1).

%  Extracting a quatified formula that is a subformula of one whose
%  principle operator is Boolean operators is OK
%  because the variables are distinct

extract(all(X, A) or B,  all(X, C)) :- !, extract(A or B,  C).
extract(ex(X,  A) or B,  ex(X,  C)) :- !, extract(A or B,  C).
extract(all(X, A) and B, all(X, C)) :- !, extract(A and B, C).
extract(ex(X,  A) and B, ex(X,  C)) :- !, extract(A and B, C).

extract(A or  all(X, B), all(X, C)) :- !, extract(A or B,  C).
extract(A or  ex(X,  B), ex(X,  C)) :- !, extract(A or B,  C).
extract(A and all(X, B), all(X, C)) :- !, extract(A and B, C).
extract(A and ex(X,  B), ex(X,  C)) :- !, extract(A and B, C).

%  If the principle operator is Boolean, recurse on the subformulas
%  If a returned subformula is quantified, recurse on the whole formula

extract(A or B, C) :- !,
  extract(A, A1),
  extract(B, B1),
  (is_quantified(A1, B1) -> extract(A1 or B1, C) ; (C = A or B) ).

extract(A and B, C) :- !,
  extract(A, A1),
  extract(B, B1),
  (is_quantified(A1, B1) -> extract(A1 and B1, C) ; (C = A and B) ).

extract(A, A).

%  Check if a formula is quantifed

is_quantified(ex(_,_),  _).
is_quantified(all(_,_), _).
is_quantified(_, ex(_,_)).
is_quantified(_, all(_,_)).


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
