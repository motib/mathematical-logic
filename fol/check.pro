% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Check Hilbert proof in first-order logic.

:- module(check, [proof/1]).

%  proof(List)
%  proof(List, Line, SoFar, Gens)
%    Succeeds if the List of formulas is a proof
%      where SoFar is a list of formulas already proven,
%      and Gens is a list of constants to which Generalization
%      has been applied (used for the proviso of deduction theorem).
%      The head of List is the Line'th line in the proof.

proof(List) :- proof(List, 0, [], []).

proof([], _, _, _).

%  Axiom
proof([Fml | Tail], Line, SoFar, Gens) :- 
  Line1 is Line + 1,
  Fml = deduce(_, A),
  axiom(A, N), !,
  write_proof_line(Line1, Fml, ['Axiom ', N]),
  proof(Tail, Line1, [Fml | SoFar], Gens).

%  Assumption
proof([Fml | Tail], Line, SoFar, Gens) :- 
  Line1 is Line + 1,
  Fml = deduce(Assump, A),
  member(A, Assump), !,
  write_proof_line(Line1, Fml, ['Assumption']),
  proof(Tail, Line1, [Fml | SoFar], Gens).

%  Deduction
proof([Fml | Tail], Line, SoFar, Gens) :- 
  Line1 is Line + 1,
  Fml = deduce(Assump, A imp B),
  nth1(L, SoFar, deduce(Previous, B)),
  member(A, Previous),
  proviso(Gens, A), !,
  delete(Previous, A, Assump),
  D is Line1 - L,
  write_proof_line(Line1, Fml, ['Deduction ', D]),
  proof(Tail, Line1, [Fml | SoFar], Gens).

%  MP
proof([Fml | Tail], Line, SoFar, Gens) :- 
  Line1 is Line + 1,
  Fml = deduce(_, A),
  nth1(L1, SoFar, deduce(_, B imp A)), 
  nth1(L2, SoFar, deduce(_, B)), !,
  MP1 is Line1 - L1,
  MP2 is Line1 - L2,
  write_proof_line(Line1, Fml, ['MP ', MP1, ',', MP2]),
  proof(Tail, Line1, [Fml | SoFar], Gens).

%  Generalization
proof([Fml | Tail], Line, SoFar, Gens) :- 
  Line1 is Line + 1,
  Fml = deduce(_, all(X, A)),
  nth1(L, SoFar, deduce(_, A1)),
  instance(A, A1, X, C), !,
  G is Line1 - L,
  write_proof_line(Line1, Fml, ['Gen ', G]),
  proof(Tail, Line1, [Fml | SoFar], [C | Gens]).

proof([Fml | _], Line, _, _) :-
  Line1 is Line + 1,
  write_proof_line(Line1, Fml, ['Cannot prove']).

%  axiom(A, N)
%    A is the N'th axiom.

axiom(A imp (_ imp A), 1).
axiom((A imp (B imp C)) imp ( (A imp B) imp (A imp C)), 2).
axiom(((neg B) imp (neg A)) imp (A imp B), 3).
axiom(all(X, A1) imp A2, 4) :-
  instance(A1, A2, X, _).
axiom(all(X, A imp B) imp (A imp all(X, B)), 5) :-
  \+ free_in(A, X).

%   proviso(Gens, A)
%     Deduction theorem can only be applied if the constants 
%     to which generalization has been applied (Gen) do
%     occur in the formula A.
%     free_in can be used since a constant is necessarily 'free'.

proviso([], _).
proviso([C | Rest], A) :-
  \+ free_in(A, C),
  proviso(Rest, A).

%  free_in(A, X)
%    X occurs free in A.

free_in(all(X, A), Y) :- \+ X==Y, free_in(A, Y).
free_in(ex(X, A),  Y) :- \+ X==Y, free_in(A, Y).
free_in(A or B, X)    :- free_in(A, X); free_in(B, X).
free_in(A and B, X)   :- free_in(A, X); free_in(B, X).
free_in(A imp B, X)   :- free_in(A, X); free_in(B, X).
free_in(A eqv B, X)   :- free_in(A, X); free_in(B, X).
free_in(A xor B,  X)  :- free_in(A, X); free_in(B, X).
free_in(neg A, X)     :- free_in(A, X).
free_in(A, X)         :- A =.. [_ | Vars], member_term(X, Vars).

member_term(L, [Head|_]) :- L == Head, !.
member_term(L, [_|Tail]) :- !, member_term(L, Tail).


