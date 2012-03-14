% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Resolution procedure for the propositional calculus

%  resolve(S) - resolve the set of clauses S.

resolve([]) :-
  write('The empty set of clauses is valid.').

resolve(S) :-
  member([], S),
  write_clauses(S), nl,
  write('The empty clause is unsatisfiable.').

resolve(S) :-
  member(C1, S),            % choose two clauses
  member(C2, S),
  clashing(C1, L1, C2, L2), % check that they clash
  delete(C1, L1, C1P),      % delete the clashing literals
  delete(C2, L2, C2P),     
  union(C1P, C2P, C),       % new clause is their union
  \+ clashing(C, _, C, _),  % don't add trivial clauses
  \+ member(C, S),          % don't add an existing clause
  write_clauses(S), nl,    
  resolve([C | S]).         % add the resolvent to the set

resolve(_) :-
  write('The set of clauses is satisfiable.').

clashing(C1, L, C2, neg L) :-
  member(L, C1),
  member(neg L, C2), !.
clashing(C1, neg L, C2, L) :-
  member(neg L, C1),
  member(L, C2),  !.
