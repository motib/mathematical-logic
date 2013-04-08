% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(display,
          [display/2, display/3, display/4, display/5, display/6]).

:- use_module([config,counters,dot,io,modes]).


%  display/2,3,4,5
%    For each option there is a separate definition of display
%    Check that the option is set before displaying
%    These predicates will not fail; at worst they do nothing


%  Two arguments

display(assignment, Assignments) :-
  check_option(assignment), !,
  write('Conflict caused by assignments:\n'),
  write_assignments(Assignments), nl.

display(backtrack, Highest) :-
  get_mode(Mode),
  Mode = ncb,
  check_option(backtrack), !,
  write('Non-chronological backtracking to level: '),
  write(Highest), nl.

display(clause, Clauses) :-
  check_option(clause), !,
  write('Clauses to be checked for satisfiability:\n'),
  write_clauses(Clauses, Clauses), nl.

display(decision, Assignment) :-
  check_option(decision), !,
  write('Decision assignment: '),
  write_assignment(Assignment, no), nl.

display(learned, Learned) :-
  check_option_not_dpll(learned), !,
  write('Learned clause: '),
  write(Learned), nl.

display(partial, Assignments) :-
  check_option(partial), !,
  write('Assignments so far:\n'),
  write_assignments(Assignments), nl.

display(result, []) :-
  check_option(result), !,
  write('Unsatisfiable:\n'),
  show_counters.

display(result, Assignments) :-
  check_option(result), !,
  write('Satisfying assignments:\n'),
  write_assignments(Assignments), nl,
  show_counters.

display(skipped, Assignment) :-
  get_mode(Mode), Mode = ncb,
  check_option(skipped), !,
  write('Skip decision assignment: '),
  write_assignment(Assignment, no), nl.

display(variable, Variables) :-
  check_option(variable), !,
  write('Variables: '),
  write(Variables), nl.

display(_, _).


%  Three arguments

display(conflict, Conflict, Clauses) :-
  check_option(conflict), !,
  write('Conflict clause: '),
  write_clause(Conflict, Clauses), nl.

display(dot, Graph, Clauses) :-
  check_option_not_dpll(dot), !,
  display_dot(Graph, Clauses).

% Generate dot graph also if dot_inc is selected
display(dot, Graph, Clauses) :-
  check_option_not_dpll(dot_inc), !,
  display_dot(Graph, Clauses).

display(dot_inc, Graph, Clauses) :-
  check_option_not_dpll(dot_inc), !,
  display_dot(Graph, Clauses).

display(graph, Graph, Clauses) :-
  check_option_not_dpll(graph), !,
  display_graph(Graph, Clauses).

% Display graph also if incremental is selected
display(graph, Graph, Clauses) :-
  check_option_not_dpll(incremental), !,
  display_graph(Graph, Clauses).

display(incremental, Graph, Clauses) :-
  check_option_not_dpll(incremental), !,
  display_graph(Graph, Clauses).

%  Display literal assignment levels only if resolvent is also chosen
display(literal, Literal, Level) :-
  check_option_not_dpll(literal),
  check_option(resolvent), !,
  write('Literal: '),
  write(Literal),
  write(' assigned at level: '),
  write(Level), nl.

display(tree, Assignments, Conflict) :-
  check_option(tree), !,
  write_tree(Assignments, Conflict).

display(tree, Assignments, Conflict) :-
  check_option(tree_inc), !,
  write_tree(Assignments, Conflict).

display(tree_inc, Assignments, Conflict) :-
  check_option(tree_inc), !,
  write_tree(Assignments, Conflict).

%  Display uip only if resolvent is also chosen
display(uip, no, Level) :-
  check_option_not_dpll(uip),
  check_option(resolvent), !,
  write('Not a UIP: two literals are assigned at level: '),
  write(Level), nl.

display(uip, yes, Level) :-
  check_option_not_dpll(uip),
  check_option(resolvent), !,
  write('UIP: one literal is assigned at level: '),
  write(Level), nl.

display(_, _, _).


%  Four arguments

%  Empty clause is a flag to prevent duplicate display from find_unit
display(evaluate, [], _, _) :- !.
display(evaluate, Clause, Reason, Literal) :-
  check_option(evaluate), !,
  write('Evaluate: '),
  write(Clause),
  write(Reason),
  display_deleted_literal(Literal).

display(unit, Literal, Unit, Clauses) :-
  check_option(unit), !,
  write('Propagate unit: '),
  write(Literal),
  write(' derived from: '),
  write_clause(Unit, Clauses), nl.

display(_, _, _, _).

display_deleted_literal(none) :- nl, !.
display_deleted_literal(Literal) :-
  write(Literal),
  write(' deleted\n').

%  Five arguments

display(resolvent, Literal, Clause, Clause1, Clause2) :-
  check_option_not_dpll(resolvent), !,
  write('Clause: '),
  write(Clause),
  write(' unsatisfied'), nl,
  write('Complement of: '), write(Literal),
  write(' assigned true in the unit clause: '),
  write(Clause1), nl,
  write('Resolvent of the two clauses: '),
  write(Clause2),
  write(' is also unsatisfiable'), nl.

display(_, _, _, _, _).

%  Six arguments

display(dominator, Path_List, Dominator, Decisions, Result, Learned) :-
  check_option_not_dpll(dominator), !,
  write('Paths from the decision node at this level to kappa:\n'),
  write_paths(Path_List),
  write('A dominator is: '),
  write_assignment(Dominator), nl, 
  write('Decisions at a lower level: '),
  write(Decisions), nl,
  write('Decisions not dominated: '),
  write(Result), nl,
  write('Learned clause from dominator: '),
  write(Learned), nl.

display(_, _, _, _, _, _).


%  check_option_not_dpll/1
%    Check option and also that the mode is not dpll

check_option_not_dpll(Option) :-
  get_mode(Mode),
  Mode \= dpll,
  check_option(Option).

% display_dot/2
%   Common processing for display dot and dot_inc
%   When label option is set, pass the list of clauses to write_dot

display_dot(Graph, Clauses) :-
  check_option(label), !,
  display_dot1(Graph, Clauses).
display_dot(Graph, _) :-
  display_dot1(Graph, []).

display_dot1(Graph, Clauses) :-
  get_file_counter(ig, N),
  write('Writing dot graph: '),
  write(N), nl,
  write_dot(Graph, Clauses).


% display_graph/2
%   Common processing for display graph and incremental
%   When label option is set, pass the list of clauses to write_graph

display_graph(Graph, Clauses) :-
  check_option(label), !,
  write('Implication graph:\n'),
  write_graph(Graph, Clauses), nl.

display_graph(Graph, _) :-
  write('Implication graph:\n'),
  write_graph(Graph, []), nl.
