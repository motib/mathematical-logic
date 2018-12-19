% Copyright 2000-2018 by M. Ben-Ari. GNU GPL. See prolog.pdf.

% Modified 2018 because of the change of the semantics of
%   numbervars/4 in the current version of SWI Prolog

%  module io

:- module(io, [
  explain/1,
  explain/2,
  clear_verbose/0,
  set_verbose/0,
  write_formula/1,
  write_formula_list/1,
  write_tt_line/3,
  write_tt_title/2,
  write_proof_line/3,
  write_tableau/1,
  write_clauses/1,
  write_unified/3,
  write_tl_tableau/4,
  write_fulfil/4,
  write_latex/1
  ]).

:- dynamic verbose/0.

set_verbose :- verbose, !.
set_verbose :- assert(verbose).

clear_verbose :- retract(verbose).

explain(S) :- verbose, !, write(S), nl.
explain(_).

explain(S, Goal) :-
  verbose, !,
  write(S),
  call(Goal),
  nl.
explain(_, _).

%  term_length(T, L) - L is the length of term T

term_length(T, L) :-
  term_to_atom(T, A),
  string_to_atom(S, A),
  string_length(S, L).

%  fieldl(T, N) - write term T left  justified in a field of length T
%  fieldr(T, N) - write term T right justified in a field of length T

fieldl(T, N) :-
  write(T),
  term_length(T, L),
  Spaces is N - L,
  tab(Spaces).

fieldr(T, N) :-
  term_length(T, L),
  Spaces is N - L,
  tab(Spaces),
  write(T).

%  write_tt_title(Fml, Atoms)
%    write the truth table title: formula and atoms

write_tt_title(Fml, Atoms) :-
  to_external(Fml, EFml),
  write(EFml),
  write('  '),
  write_tt_title1(Atoms).

write_tt_title1([A|Tail]) :-
  write(A),
  write(' '),
  write_tt_title1(Tail).
write_tt_title1([]) :-
  write(' value '), nl.

%  write_tt_line(Fml, V, TV)
%    write a truth table line,
%    Fml is a formula (used for indenting)
%    V a valuation and TV the truth value

write_tt_line(Fml, V, TV) :-
  to_external(Fml, EFml),
  term_length(EFml, L),
  tab(L),
  write('  '),
  write_valuation(V),
  write('   '),
  write(TV),
  nl.

%  write_valuation(List)
%    write the valuations in the List with blank separators

write_valuation([]).                                                                       
write_valuation([(_,TV)|Tail]) :-
  write(TV),
  write(' '),
  write_valuation(Tail).

%  write_proof_line(Line, A, Reason)
%    A is the Line'th line of the proof for Reason.

write_proof_line(Line, Fml, Reason) :-
  fieldr(Line, 3),
  write('.'),
  Fml = deduce(Assump, A),
  write_formula_list(Assump),
  write(' |- '),
  to_external(A, E),
  write_formula(E),
  line_position(user_output, Pos),
  Spaces = 60 - Pos,
  tab(Spaces),
  write_list(Reason), nl.

%  write_formula(Fml) - write the formula Fml.

write_formula(Fml) :-
  copy_term(Fml, Fml1),          % Do not instantiate original formula.
  numbervars(Fml1, 1, _, [functor_name(x)]),
%  numbervars(Fml1, x, 1, _),     % Instantiate variables for output.
  write_formula1(Fml1).

%  Quantifiers
write_formula1(all(X, A)) :- !,
  write('A'),
  write_term(X),
  write_formula1(A).
write_formula1(ex(X, A))  :- !,
  write('E'),
  write_term(X),
  write_formula1(A).

%  Binary operators
write_formula1(Fml) :-
  Fml =.. [Opr, A, B],
  symbol_opr(Opr, _), !,
  write_formula2(Opr, A, B).

%  Unary operators
write_formula1(Fml) :-
  Fml =.. [Opr, A],
  symbol_opr(Opr, _), !,
  write(Opr),
  write_formula1(A).

%  Atoms
write_formula1(A) :-
  atom(A), !,
  write(A).

%  Terms
write_formula1(A) :-
  A =.. [F | Vars],
  write(F), write('('),
  write_subterms(Vars),
  write(')').

%  write_formula2(Fml) - add parentheses for binary formulas

write_formula2(Op, A, B) :-
  write('('), write_formula1(A), write(' '),
  write(Op),
  write(' '), write_formula1(B), write(')').

%  write_term(T)        - write the term T
%  write_subterms(List) - write the list of subterms

write_term(T)    :-
  atom(T), !,
  write(T).
write_term(x(N)) :- !,
  write('x'), write(N).
write_term(T) :-
  T =.. [F | Subterms],
  write_term(F),
  write('('),
  write_subterms(Subterms),
  write(')').
write_term(T) :- write(T).

write_subterms(List) :- write_comma_list(write_term, List).

%  write_list(List) - write a list

write_list(List) :- checklist(write, List).

%  write_comma_list(Pred, List)
%    Write the elements of List with comma separators.
%    The elements are written by Pred.

write_comma_list(Pred, [T1, T2 | Tail]) :- !,
  call(Pred, T1),
  write(','),
  write_comma_list(Pred, [T2 | Tail]).
write_comma_list(Pred, [T]) :-
  call(Pred, T).
write_comma_list(_, []).

%  write_formula_list - write list of formula separated by commas

write_formula_list(List) :-
  to_external_list(List, ListE),
  write_comma_list(write_formula, ListE).

%  write_tableau(Tab)
%  write_tableau(Tab, Indent, Line)
%    Write tableau Tab, Indenting each level of the tree,
%      with line number Line (currently only for temporal logic).
%  write_formula_list(Fml)
%    Write the list of formulas Fml.
 
write_tableau(Tab) :-
  write_tableau(Tab, 0, 0).

write_tableau(empty,_,_).
write_tableau(closed,_,_) :- 
  write(' Closed').
write_tableau(open,_,_)   :- 
  write(' Open  ').
write_tableau(connect(N),_,_) :-
  write(' Connect to '), write(N).
write_tableau(t(Fmls, Left, Right), Indent, N) :-
  nl,
  ( (N =\= 0) -> (fieldr(N, 3), write('  ')) ; true),
  tab(Indent), Indent1 is Indent + 3,
  write_formula_list(Fmls),
  write_tableau(Left, Indent1, N),
  write_tableau(Right, Indent1, N).

%  Clause for first-order with extra field.

write_tableau(t(Fmls, Left, Right, _), Indent, N) :-
  write_tableau(t(Fmls, Left, Right), Indent, N).

%  Clause for temporal logic with extra fields.

write_tableau(t(Fmls, Left, Right, N, _), Indent, _) :-
  write_tableau(t(Fmls, Left, Right), Indent, N).

%  write_clauses(List) - write a list of clauses
%  write_clauses - writes an empty list as []
%    and passes non-empty lists to write_clause1

write_clauses([]) :-
  write('[]').
write_clauses(List) :-
  copy_term(List, List1),         % Do not instantiate original formula.
  numbervars(List1, 1, _, [functor_name(x)]),
%  numbervars(List1, x, 1, _),     % Instantiate variables for output.
  write('['),
  write_clauses1(List1).

write_clauses1([H]) :-
  to_external_list(H, HE),
  write('['),
  write_clause(HE),
  write(']').
write_clauses1([H|T]) :-
  to_external_list(H, HE),
  write('['),
  write_clause(HE),
  write(','),
  write_clauses1(T).

write_clause([]) :-
  write(']').
write_clause([H]) :-
  write_formula(H),
  write(']').
write_clause([H|T]) :-
  write_formula(H),
  write(','),
  write_clause(T).

write_unified(L1, L2, Subst) :-
  numbervars((L1,L2), 1, _, [functor_name(x)]),
%  numbervars((L1, L2), x, 1, _),
  write('Unify '),    write_formula(L1), nl,
  write(' and  '),    write_formula(L2), nl, nl,
  write_solved(Subst).

write_solved([failure3 | E]) :- !,
  write('Failed (different functors) in '), nl,
  write_eq_list(E).
write_solved([failure4 | E]) :- !,
  write('Failed (occur check) in '), nl,
  write_eq_list(E).
write_solved(L) :-
  write_eq_list(L).

write_eq_list(List) :- checklist(write_eq, List).

write_eq(T1 eq T2) :-
  write_term(T1), write(' = '), write_term(T2), nl.

%  write_tl_tableau -
%    Write the various elements of the tableau construction.
%  write_tau(T) -
%    Write a new line after each transition tau
%  write_tableau_result -
%    Write the result: un/satisfiable or need to check fulfilment.

write_tl_tableau(Tab, States, Tau, Tab_Result) :-
  write_tableau(Tab),   nl, nl,
  write_states(States), nl,
  checklist(write_tau, Tau), nl,
  write_tableau_result(Tab_Result).

write_tau(T) :- write(T), nl.

write_tableau_result(open)    :- write('Formula is satisfiable  '), nl.
write_tableau_result(closed)  :- write('Formula is unsatisfiable'), nl.
write_tableau_result(connect) :- write('Checking fulfilment.....'), nl.

%  write_states(List)
%    Write the List of states.

write_states([]).
write_states([st(Fmls,N)|Tail]) :- 
  write('State '), write(N), write(' '), nl, write('  '),
  write_formula_list(Fmls), nl, 
  write_states(Tail).

%  write_fulfil
%    Write the results of the fulfilment check at a connect node:
%      the SCCs and Edges
%  write_fulfil1/write_fulfil2
%    For each node, write if it is fulfiling and if not, for which formula.
%  write_fulfil_result
%    Write open if fulfiling or closed.

write_fulfil(connect, SCCs, Edges, Fulfil_Result) :-
  write('SCCs = '),  write(SCCs), nl,
  write('Edges = '), write(Edges), nl,
  write_fulfil1(Fulfil_Result),
  write_fulfil_result(Fulfil_Result).
write_fulfil(_, _, _, _).

write_fulfil1([Head|Tail]) :-
  write_fulfil2(Head),
  write_fulfil1(Tail).
write_fulfil1([]).

write_fulfil2(none) :- !.
write_fulfil2(ok(S))   :- !,
  write(S), write(' is fulfilling'), nl.
write_fulfil2(notok(S,F)) :- !,
  write(S), write(' is not fulfilling for '),
  to_external(F, FE),
  write_formula(FE), nl.

write_fulfil_result(R) :-
  member(ok(_), R), !,
  write_tableau_result(open).
write_fulfil_result(_) :-
  write_tableau_result(closed).


  
%  write_latex(Fml) - write the formula Fml in LaTex

write_latex(Fml) :-
  copy_term(Fml, Fml1),          % Do not instantiate original formula.
  numbervars(Fml1, 1, _, [functor_name(x)]),
%  numbervars(Fml1, x, 1, _),     % Instantiate variables for output.
  write_latex1(Fml1).

%  Quantifiers
write_latex1(all(X, A)) :- !,
  latex_opr(Q, 'forall'),
  write(Q),
  write(' '),
  write_term(X),
  write(' '),
  write_latex1(A).
write_latex1(ex(X, A))  :- !,
  latex_opr(Q, 'exists'),
  write(Q),
  write(' '),
  write_term(X),
  write(' '),
  write_latex1(A).

%  Binary operators
write_latex1(Fml) :-
  Fml =.. [Opr, A, B],
  latex_opr(L, Opr), !,
  write_latex2(L, A, B).

%  Unary operators
write_latex1(Fml) :-
  Fml =.. [Opr, A],
  latex_opr(L, Opr), !,
  write(L),
  write(' '),
  write_latex1(A).

%  Atoms
write_latex1(A) :-
  atom(A), !,
  write(A).

%  Terms
write_latex1(A) :-
  A =.. [F | Vars],
  write(F), write('('),
  write_subterms(Vars),
  write(')').

%  write_latex2(Fml) - add parentheses for binary formulas

write_latex2(Op, A, B) :-
  write('('), write_latex1(A), write(' '),
  write(Op),
  write(' '), write_latex1(B), write(')').
