% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Display explanations

:- module(display, [display/2, display/3, display/4]).

:- use_module([config,counters,io,modes]).


%  display/2,3,4
%    For each option there is a separate definition of display
%    Check that the option is set before displaying

%  Two arguments

display(assignments, Assignments) :-
  check_option(assignments), !,
  write('Conflict caused by assignments:\n'),
  write_assignments(Assignments), nl.

display(backtrack, Highest) :-
  get_mode(Mode), Mode = ncb,
  check_option(backtrack), !,
  write('Non-chronological backtracking to level: '),
  write(Highest), nl.

display(clauses, Clauses) :-
  check_option(clauses), !,
  write('Clauses to be checked for satisfiability:\n'),
  write_clauses(Clauses, Clauses), nl.

display(decision, Assignment) :-
  check_option(decision), !,
  write('Decision assignment: '),
  write_assignment(Assignment), nl.

display(learned, Learned) :-
  get_mode(Mode), Mode \= dpll,
  check_option(learned), !,
  write('Learned clause: '),
  write(Learned), nl.

display(literal, Literal) :-
  get_mode(Mode), Mode \= dpll,
  check_option(literal), !,
  write('Literal assigned at this level: '),
  write(Literal), nl.

display(partial, Assignments) :-
  check_option(partial), !,
  write('Assignments so far:\n'),
  write_assignments(Assignments), nl.

display(skipping, Assignment) :-
  get_mode(Mode), Mode = ncb,
  check_option(skipping), !,
  write('Skipping alternative to: '),
  write_assignment(Assignment), nl.

display(variables, Variables) :-
  check_option(variables), !,
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
  write('Writing dot graph: '),
  write(N), nl,
  write_dot(Graph, Clauses).

display(graph, Graph, Clauses) :-
  get_mode(Mode), Mode \= dpll,
  check_option(graph), !,
  write('Implication graph (final):\n'),
  (check_option(labels) -> Clauses1 = Clauses ; Clauses1 = []),
  write_graph(Graph, Clauses1), nl,
  display(dot, Graph, Clauses1).

display(incremental, Graph, Clauses) :-
  get_mode(Mode), Mode \= dpll,
  check_option(incremental), !,
  write('Implication graph (incremental):\n'),
  (check_option(labels) -> Clauses1 = Clauses ; Clauses1 = []),
  write_graph(Graph, Clauses1), nl,
  display(dot, Graph, Clauses1).

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
  write('Not a UIP because two literals are assigned at this level: '),
  write(Level), nl.

display(uip, yes, Level) :-
  check_option(uip), !,
  write('UIP because one literal is assigned at this level: '),
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

display(resolvent, Clause, Clause1, Clause2) :-
  get_mode(Mode), Mode \= dpll,
  check_option(resolvent), !,
  write('Resolvent of: '),
  write(Clause),
  write(' with: '),
  write(Clause1),
  write(' is: '),
  write(Clause2), nl.

display(unit, Literal, Unit, Clauses) :-
  check_option(unit), !,
  write('Propagate unit: '),
  write(Literal),
  write(' derived from: '),
  write_clause(Unit, Clauses), nl.

display(_, _, _, _).
