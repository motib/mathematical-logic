% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

% Resolution theorem prover

%  resolve(S) - resolve the set of clauses S.

resolve([]) :- 
  write('The empty set of clauses is valid.').

resolve(S)  :-
  member([], S),
  write_clauses(S), nl,
  write('The set contains the empty clause so it is unsatisfiable.').

resolve(S)  :-
  member(C1, S),         % Choose two clauses.
  member(C2, S),
  C1 \== C2,             % Make sure they are different               
  copy_term(C2, C2_R),   % Standardize apart by renaming variables.
                         % Check that they are clashing.
  clashing(C1, L1, C2_R, L2, Subst),
                         % Create the resolvent (C1-L1)u(C2-L2).
  delete_lit(C1, L1, Subst, C1P),
  delete_lit(C2_R, L2, Subst, C2P),
  clause_union(C1P, C2P, Resolvent),
                         % Check that resolvent does not clash with itself.
  \+ clashing(Resolvent, _, Resolvent, _, _),
                         % Check that resolvent does not already exist.
  \+ member(Resolvent, S),
  write('Resolve '),     write(C1),
  write(' with '),       write(C2),
  write(' renamed as '), write(C2_R), nl,
  write('with subsitution '), write(Subst), nl,
  write('to give '),     write(Resolvent), nl, nl,
                         % Add resolvent to set and continue.
  resolve([Resolvent | S]).

resolve(_) :-
  write('The set of clauses is satisfiable.').


%  clashing(C1, L1, C2, L2, Subst) -
%    Literals clash if one is the negation of the other and
%      they unify. Return the mgu substitution.

clashing(C1, L1, C2, neg L2, Subst) :-
  member(L1, C1),
  member(neg L2, C2),
  unify(L1, L2, Subst),
  !.
clashing(C1, neg L1, C2, L2, Subst) :-
  member(neg L1, C1),
  member(L2, C2),
  unify(L1, L2, Subst),
  !.

%  delete_lit(C, L, Substution, Result)
%    In clause C, delete all occurrences of literal L
%      after performing Subsitution, and return Result.
%    Perform substitution and call:
%  delete_lit1(C, L, Result)

delete_lit(C, L, Subst, Result) :-
  apply_subst(C, C1, Subst),
  apply_subst([L], [L1], Subst),
  delete_lit1(C1, L1, Result).

delete_lit1([Head|Tail], L, Result) :-
  Head == L, !,
  delete_lit1(Tail, L, Result).
delete_lit1([Head|Tail], L, [Head|Result]) :- !,
  delete_lit1(Tail, L, Result).
delete_lit1([], _, []).

%  clause_union(C1, C2, Result)
%    Perform set union of C1, C2,
%      using member_term so as not to instantiate variables.

clause_union([Head|Tail], C, Result) :-
  member_term(Head, C), !,
  clause_union(Tail, C, Result).
clause_union([Head|Tail], C, [Head|Result]) :- !,
  clause_union(Tail, C, Result).
clause_union([], C, C).

member_term(L, [Head|_]) :- L == Head, !.
member_term(L, [_|Tail]) :- !, member_term(L, Tail).

%  apply_subst(List, NewList, Substitution)
%    Apply Substitution to all elements of List to give NewList.

apply_subst([], [], _).
apply_subst([Head|Tail], [Head1|Tail1], Subst) :-
  subst_for_var(Head, Head1, Subst),
  apply_subst(Tail, Tail1, Subst).

%  subst_for_var(Term, NewTerm, Substitution)
%    Apply Substitution for all variables in Term to give NewTerm.
%    If compound term, decompose and call apply_subst.

subst_for_var(Term, Term1, Subst) :-
  nonvar(Term),
  Term  =.. [F|Args], !,
  apply_subst(Args, Args1, Subst),
  Term1 =.. [F|Args1].
subst_for_var(V, V2, [V1 eq V2 | _]) :-
  V == V1, !.
subst_for_var(V, V2, [_ | Tail]) :-
  subst_for_var(V, V2, Tail).
subst_for_var(V, V, []).
