% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Create a truth table

%  create_tt(Fml) creates the truth table for the formula Fml
%    get_atoms gets a list Atoms of the atoms in Fml
%    generate creates a valuation for the Atoms
%       a valuation is a list of pairs (A,TV), where
%        A is a propositional symbol and
%        TV is the value assigned to it
%    tt computes the truth value of a formula under a valuation
%    write_tt_line print a line of the truth table
%    
%    failure is used to backtrack so that generate can
%      create another valuation

create_tt(Fml) :-
  to_internal(Fml, IFml),
  get_atoms(IFml, Atoms),
  write_tt_title(IFml, Atoms),
  generate(Atoms, V),
  tt(IFml, V, TV),
  write_tt_line(IFml, V, TV),
  fail.
create_tt(_).

%  get_atoms(Fml, Atoms) returns a sorted list of the Atoms in Fml

get_atoms(Fml, Atoms) :-
  get_atoms1(Fml, UnSorted),
  sort(UnSorted, Atoms).

%  get_atoms1(Fml, Atoms)
%    decompose the formula to get the atoms

get_atoms1(Fml, Atom) :-
  Fml =.. [_, A, B], !,
  get_atoms1(A, Atom1),
  get_atoms1(B, Atom2),
  union(Atom1, Atom2, Atom).
get_atoms1(Fml, Atom) :-
  Fml =.. [_, A], !,
  get_atoms1(A, Atom).
get_atoms1(A, [A]).

%  generate(Atoms, V)
%    for each atom A in the list Atoms,
%    generate a pair, first (A,t) and upon backtracking (A,f)

generate([A | ATail], [(A,t) | VTail]) :-
  generate(ATail, VTail).
generate([A | ATail], [(A,f) | VTail]) :-
  generate(ATail, VTail).
generate([], []).

%  tt(Fml, V, TV)
%    returns in TV the truth value of the formula Fml under the evaluation V

tt(A eqv B, V, TV) :- tt(A, V, TVA), tt(B, V, TVB), opr(eqv, TVA, TVB, TV).
tt(A xor B, V, TV) :- tt(A, V, TVA), tt(B, V, TVB), opr(xor, TVA, TVB, TV).
tt(A imp B, V, TV) :- tt(A, V, TVA), tt(B, V, TVB), opr(imp, TVA, TVB, TV).
tt(A or  B, V, TV) :- tt(A, V, TVA), tt(B, V, TVB), opr(or,  TVA, TVB, TV).
tt(A and B, V, TV) :- tt(A, V, TVA), tt(B, V, TVB), opr(and, TVA, TVB, TV).
tt(neg A,   V, TV) :- tt(A, V, TVA), negate(TVA, TV).
tt(A,       V, TV) :- member((A,TV), V).
