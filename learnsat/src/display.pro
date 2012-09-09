% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Display explanations

:- module(display,
          [display/2, display/3, display/4, display/5, display/6]).

:- use_module([config,counters,io,modes]).


%  display/2,3,4,5
%    For each option there is a separate definition of display
%    Check that the option is set before displaying

%  Two arguments

display(assignment, Assignments) :-
  check_option(assignment), !,
  write('Conflict caused by assignments:\n'),
  write_assignments(Assignments), nl.

display(backtrack, Highest) :-
  get_mode(Mode), Mode = ncb,
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
  get_mode(Mode), Mode \= dpll,
  check_option(learned), !,
  write('Learned clause: '),
  write(Learned), nl.

display(partial, Assignments) :-
  check_option(partial), !,
  write('Assignments so far:\n'),
  write_assignments(Assignments), nl.

display(skipping, Assignment) :-
  get_mode(Mode), Mode = ncb,
  check_option(skipping), !,
  write('Skip decision assignment: '),
  write_assignment(Assignment, no), nl.

display(variable, Variables) :-
  check_option(variable), !,
  write('Variables: '),
  write(Variables), nl.

%  Don't fail if the option is not set or not relevant
  display(_, _).


%  Three arguments

display(conflict, Conflict, Clauses) :-
  check_option(conflict), !,
  write('Conflict clause: '),
  write_clause(Conflict, Clauses), nl.

display(dot, Graph, Clauses) :-
  get_mode(Mode), Mode \= dpll,
  check_option(dot), !,
  get_file_counter(N),
  write('Writing dot graph (final): '),
  write(N), nl,
  (check_option(label) -> Clauses1 = Clauses ; Clauses1 = []),
  write_dot(Graph, Clauses1).

display(dot_inc, Graph, Clauses) :-
  get_mode(Mode), Mode \= dpll,
  check_option(dot_inc), !,
  get_file_counter(N),
  write('Writing dot graph (incremental): '),
  write(N), nl,
  (check_option(label) -> Clauses1 = Clauses ; Clauses1 = []),
  write_dot(Graph, Clauses1).

display(graph, Graph, Clauses) :-
  get_mode(Mode), Mode \= dpll,
  check_option(graph), !,
  write('Implication graph (final):\n'),
  (check_option(label) -> Clauses1 = Clauses ; Clauses1 = []),
  write_graph(Graph, Clauses1), nl.

display(incremental, Graph, Clauses) :-
  get_mode(Mode), Mode \= dpll,
  check_option(incremental), !,
  write('Implication graph (incremental):\n'),
  (check_option(label) -> Clauses1 = Clauses ; Clauses1 = []),
  write_graph(Graph, Clauses1), nl.

display(literal, Literal, Level) :-
  get_mode(Mode), Mode \= dpll,
  check_option(literal), !,
  write('Literal: '),
  write(Literal),
  write(' assigned at level: '),
  write(Level), nl.

display(result, satisfiable, Assignments) :-
  check_option(result), !,
  write('Satisfying assignments:\n'),
  write_assignments(Assignments), nl,
  show_counters.

display(result, unsatisfiable, _) :-
  check_option(result), !,
  write('Unsatisfiable:\n'),
  show_counters.

display(uip, no, Level) :-
  get_mode(Mode), Mode \= dpll,
  check_option(uip), !,
  write('Not a UIP: two literals are assigned at level: '),
  write(Level), nl.

display(uip, yes, Level) :-
  check_option(uip), !,
  write('UIP: one literal is assigned at level: '),
  write(Level), nl.

display(_, _, _).


%  Four arguments

%  Empty clause is a flag to prevent duplicate evaluation from find_unit
display(evaluate, [], _, _) :- !.
display(evaluate, Clause, Reason, Literal) :-
  check_option(evaluate), !,
  write('Evaluate: '),
  write(Clause),
  write(Reason),
  (Literal \= none -> write(Literal), write(' deleted') ; true),
  nl.

display(unit, Literal, Unit, Clauses) :-
  check_option(unit), !,
  write('Propagate unit: '),
  write(Literal),
  write(' derived from: '),
  write_clause(Unit, Clauses), nl.

display(_, _, _, _).

%  Five arguments

display(resolvent, Literal, Clause, Clause1, Clause2) :-
  get_mode(Mode), Mode \= dpll,
  check_option(resolvent), !,
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
  check_option(dominator), !,
  write('Paths from decision node at this level to kappa:\n'),
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
