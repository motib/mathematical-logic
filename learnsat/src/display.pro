% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Display explanations
%  Set/clear display options and algorithm mode 

:- module(display, [
  display/2, display/3, display/4,
  set_display/1, clear_display/1, init_display/0, usage/0,
  set_mode/1, get_mode/1, init_mode/0,
  show_config/0]).

:- ensure_loaded([config,counters,io]).

%  show_config/0
%    Display version, default mode and display options
%      and current mode and display options

show_config :-
  version(V),
  write('LearnSat version: '),
  write(V), nl,
  default_mode(M1),
  write('Default mode: '),
  write(M1), nl,
  default_display(D1),
  write('Default display options:\n'),
  write(D1), nl,
  mode(M2),
  write('Current mode: '),
  write(M2), nl,
  findall(D, display_option(D), List),
  write('Current display options:\n'),
  write(List), nl.
show_config.

%  LearnSAT can be run in three modes;
%    dpll - DPLL algorithm
%    cdcl -   with conflict-directed clause learning
%    ncb  -   also with non-chronological backtracking

:- dynamic mode/1.

%  init_mode/0, set_mode/1, get_mode/1
%    Mode - dpll (default) or cdcl or ncb

%  If there is already a mode, don't change it
init_mode :-
  retract(mode(Mode)), !,
  assert(mode(Mode)).
init_mode :-
  assert(mode(dpll)).

set_mode(dpll) :- !,
  retractall(mode(_)),
  assert(mode(dpll)).
set_mode(cdcl) :- !,
  retractall(mode(_)),
  assert(mode(cdcl)).
set_mode(ncb) :- !,
  retractall(mode(_)),
  assert(mode(ncb)).
set_mode(X) :-
  write('Mode '), write(X), write(' not recognized.\n'),
  write('Run "usage" for a list of modes.\n').

get_mode(Mode) :-
  mode(Mode), !.
get_mode(dpll).


:- dynamic display_option/1.


%  usage/0 - print usage documentation

usage :-
  write('LearnSAT v1.0.0. Copyright 2012 by Moti Ben-Ari. GNU GPL.\n'),
  write('  dpll(Clauses, Decisions)\n'),
  write('    Clauses:   a list of list of literals (p or ~p)\n'),
  write('    Decisions: satisfying assignments or [] if unsatisfiable\n'),
  write('  set_mode(Mode), where Mode is one of:\n'),
  write('    dpll: DPLL algorithm (default)\n'),
  write('    cdcl: DPLL with conflict-directed clause learning\n'),
  write('    ncb:  CDCL with non-chronological backtracking\n'),
  write('  set_display(D), clear_display(D),\n'),
  write('        where D is all, or default (* below),\n'),
  write('        or one of, or a list of one or more of:\n'),
  write('    [assignments] assignments that caused a conflict\n'),
  write('    [backtrack]*  level of non-chronological backtracking\n'),
  write('    [clauses]     clauses to be checked for satisfiability\n'),
  write('    [conflict]*   conflict clause\n'),
  write('    [decision]*   decision assignments\n'),
  write('    [dot]         implication graph in dot format\n'),
  write('    [graph]       implication graph\n'),
  write('    [incremental] incremental build of the implication graph\n'),
  write('    [learned]*    learned clause\n'),
  write('    [literal]     literals found assigned during CDCL\n'),
  write('    [resolvent]   resolvents created during CDCL\n'),
  write('    [result]*     result of the algorithm with statistics\n'),
  write('    [skipping]    assignments skipped when backtracking\n'),
  write('    [uip]         unique implication point\n'),
  write('    [unit]*       unit clauses\n'),
  write('    [variables]   variables that are not assigned so far\n').


%  all_display/1, default_display/1, init_display/0
%    Options - set the list of all options and the default options
%      none is a dummy option to distinguish initialization from none

all_display([
  assignments, backtrack, clauses, conflict, decision, dot, graph,
  incremental, learned, literal, none, resolvent, result, skipping, uip,
  unit, variables]).

%  If there are already display options, don't change them
init_display :-
  retract(display_option(Option)), !,
  assert(display_option(Option)).
init_display :-
  default_display(List),
  set_display(List).


%  display/2,3,4
%    For each option there is a separate definition of display
%    Check that the option is set before displaying

%  Two arguments

display(assignments, Assignments) :-
  display_option(assignments), !,
  write('Conflict caused by assignments:\n'),
  write_assignments(Assignments), nl.

display(backtrack, Highest) :-
  get_mode(Mode), Mode = ncb,
  display_option(backtrack), !,
  write('Non-chronological backtracking to level '),
  write(Highest), nl.

display(clauses, Clauses) :-
  display_option(clauses), !,
  write('Clauses to be checked:\n'),
  write_clauses(Clauses, Clauses), nl.

display(decision, Assignment) :-
  display_option(decision), !,
  write('Decision assignment: '),
  write_assignment(Assignment), nl.

display(dot, Graph) :-
  get_mode(Mode), Mode \= dpll,
  display_option(dot), !,
  write('Writing dot graph\n'),
  write_dot(Graph).

display(graph, Graph) :-
  get_mode(Mode), Mode \= dpll,
  display_option(graph), !,
  write('Implication graph (final):\n'),
  write_graph(Graph), nl.

display(incremental, Graph) :-
  get_mode(Mode), Mode \= dpll,
  display_option(incremental), !,
  write('Implication graph (incremental):\n'),
  write_graph(Graph), nl.

display(learned, Learned) :-
  get_mode(Mode), Mode \= dpll,
  display_option(learned), !,
  write('Learned clause: '),
  write(Learned), nl.

display(literal, Literal) :-
  get_mode(Mode), Mode \= dpll,
  display_option(literal), !,
  write('Literal assigned at this level: '),
  write(Literal), nl.

display(skipping, Assignment) :-
  get_mode(Mode), Mode = ncb,
  display_option(skipping), !,
  write('Skipping alternative to: '),
  write_assignment(Assignment), nl.

display(uip, no) :-
  get_mode(Mode), Mode \= dpll,
  display_option(uip), !,
  write('Not a UIP because two literals are assigned at this level\n').

display(uip, yes) :-
  display_option(uip), !,
  write('UIP because one literal is assigned at this level\n').

display(variables, Variables) :-
  display_option(variables), !,
  write('Variables: '),
  write(Variables), nl.

%  Don't fail if the option is not set
  display(_, _).

%  Three arguments

display(conflict, Conflict, Clauses) :-
  display_option(conflict), !,
  write('Conflict clause: '),
  write_clause(Conflict, Clauses), nl.

display(result, satisfiable, Assignments) :-
  display_option(result), !,
  write('Satisfying assignments:\n'),
  write_assignments(Assignments), nl,
  show_counters, nl.

display(result, unsatisfiable, _) :-
  display_option(result), !,
  write('Unsatisfiable\n'),
  show_counters, nl.

display(_, _, _).

%  Four arguments

display(resolvent, Clause, Clause1, Clause2) :-
  get_mode(Mode), Mode \= dpll,
  display_option(resolvent), !,
  write('Resolvent of '),
  write(Clause),
  write(' with '),
  write(Clause1),
  write(' is '),
  write(Clause2), nl.

display(unit, Literal, Unit, Clauses) :-
  display_option(unit), !,
  write('Propagate unit '),
  write(Literal),
  write(' derived from: '),
  write_clause(Unit, Clauses), nl.

display(_, _, _, _).


%  set_display/1, clear_display/1
%    Set or clear display options
%    For all or default, recurse with the list of options

set_display(all) :- !,
  all_display(List),
  set_display(List).

set_display(default) :- !,
  default_display(List),
  set_display(List).

set_display([Head | Tail]) :- !,
  set_display1(Head),
  set_display(Tail).

set_display([]).

set_display(Option) :-
  set_display([Option]), !.

set_display(X) :-
  not_recognized(X).


clear_display(all) :- !,
  all_display(List),
  clear_display(List),
  set_display(none).

clear_display(default) :- !,
  default_display(List),
  clear_display(List).

clear_display([Head | Tail]) :- !,
  clear_display1(Head),
  clear_display(Tail).

clear_display([]).

clear_display(Option) :-
  clear_display([Option]), !.

clear_display(X) :-
  not_recognized(X).


%  non_recognized/1
%    Print an error message if the option is not recognized

not_recognized(X) :-
  write('Display option '), write(X), write(' not recognized.\n'),
  write('Run "usage" for a list of options.\n').


%  set_display1/1, clear_display1
%    Set or clear an individual option

%  Already set, don't do anything
set_display1(Option) :-
  display_option(Option), !.

%  Otherwise, assert the option
set_display1(Option) :-
  all_display(List),
  member(Option, List), !,
  assert(display_option(Option)).

set_display1(Option) :-
  not_recognized(Option).

%  If retract succeeds, the option is cleared
clear_display1(Option) :-
  retract(display_option(Option)), !.

%  If retract doesn't succeed, check that the option is recognized 
clear_display1(Option) :-
  all_display(List),
  member(Option, List), !.

clear_display1(Option) :-
  not_recognized(Option).
