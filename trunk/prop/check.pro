% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Check a Hilbert proof in the propositional logic

%  proof(List)
%  proof(List, Line, SoFar)
%    Succeeds if the List of formulas is a proof
%      where SoFar is a list of formulas already proven.
%      A formula is of the form deduce(A,F),
%        where A is a list of assumptions and F is the formula.
%
%    The head of List is the Line'th line in the proof,
%      so the N'th element of the list is the Line-N'th line.

proof(List) :- proof(List, 0, []).

proof([], _, _).

proof([Fml | Tail], Line, SoFar) :-     % for an axiom
  Line1 is Line + 1,
  Fml = deduce(_, A),
  axiom(A, N),
  write_proof_line(Line1, Fml, ['Axiom ', N]),
  proof(Tail, Line1, [Fml | SoFar]).

proof([Fml | Tail], Line, SoFar) :-     % for an assumption
  Line1 is Line + 1,
  Fml = deduce(Assump, A),
  member(A, Assump),
  write_proof_line(Line1, Fml, ['Assumption']),
  proof(Tail, Line1, [Fml | SoFar]).

proof([Fml | Tail], Line, SoFar) :-     % for a deduction
  Line1 is Line + 1,
  Fml = deduce(Assump, A imp B),
  nth1(L, SoFar, deduce(Previous, B)),  % L'th formula in SoFar
  member(A, Previous),
  delete(Previous, A, Assump),          % delete from assumptions
  N is Line1 - L,                       % N'th line of proof
  write_proof_line(Line1, Fml, ['Deduction ', N]),
  proof(Tail, Line1, [Fml | SoFar]).

proof([Fml | Tail], Line, SoFar) :-     % for MP
  Line1 is Line + 1,
  Fml = deduce(_, A),
  nth1(L1, SoFar, deduce(_, B imp A)),  % L1'th formula is SoFar
  nth1(L2, SoFar, deduce(_, B)),        % L2'th formula is SoFar
  N1 is Line1 - L1,                     % N1'th line of proof
  N2 is Line1 - L2,                     % N2'th line of proof
  write_proof_line(Line1, Fml, ['MP ', N1, ',', N2]),
  proof(Tail, Line1, [Fml | SoFar]).

%  axiom(A, N)
%    A is the N'th axiom

axiom(A imp (_ imp A), 1).
axiom((A imp (B imp C)) imp ( (A imp B) imp (A imp C)), 2).
axiom(((neg B) imp (neg A)) imp (A imp B), 3).
