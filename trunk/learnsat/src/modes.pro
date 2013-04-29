% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Set/clear display options and algorithm modes
%  Show the configuration
%  Show the "usage" message

:- module(modes, [
  set_display/1, clear_display/1, init_display/0, check_option/1,
  set_mode/1, alg_mode/1, not_dpll_mode/0, init_modes/0,
  set_learn_mode/1, learn_mode/1,
  usage/0, display_copyright_notice/0,
  show_config/0]).

:- use_module([config, io]).

% Algorithmic mode and learn mode
%   These are exported

:- dynamic alg_mode/1, learn_mode/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Algorithmic and clause-learning modes

%  LearnSAT can be run in three modes;
%    dpll -   DPLL algorithm
%    cdcl -   DPLL with conflict-directed clause learning (CDCL)
%    ncb  -   DPLL with CDCL and non-chronological backtracking (NCB)

%  set_mode/1 - Set a new mode

set_mode(Mode) :-
  retractall(alg_mode(_)),
  check_mode(Mode),
  assert(alg_mode(Mode)).


%  check_mode/1 - Check that the mode is legal or write error

check_mode(dpll).
check_mode(cdcl).
check_mode(ncb).
check_mode(X) :-
  write('Mode "'), write(X), write('" not recognized\n'),
  write('Run "usage" for a list of modes\n').

%  not_dpll_mode/0
%    Succeeds if not dpll mode

not_dpll_mode :-
  alg_mode(Mode),
  Mode \= dpll, !.


%  Clauses can be learned by finding a dominator or by resolution
%    They might not be the same depending on the order of resolution
%    learn_mode determines which algorithm is used

%  set_learn_mode/1 - Set a new learn mode

set_learn_mode(Mode) :-
  retractall(learn_mode(_)),
  check_learn_mode(Mode),
  assert(learn_mode(Mode)).


%  check_learn_mode/1 - Check that the mode is legal or write error

check_learn_mode(resolution).
check_learn_mode(dominator).
check_learn_mode(X) :-
  write('Learn mode "'), write(X), write('" not recognized\n'),
  write('Run "usage" for a list of learn modes\n').


%  init_modes/0, init_mode/0, init_learn_mode/0
%    If no mode or learn_mode, assert the defaults from config

init_modes :-
  init_mode,
  init_learn_mode.

init_mode :-
  alg_mode(_), !.
init_mode :-
  default_alg_mode(Default),
  assert(alg_mode(Default)).

init_learn_mode :-
  learn_mode(_), !.
init_learn_mode :-
  default_learn_mode(Default),
  assert(learn_mode(Default)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Display options
  
%  Database of display options
%    "none" is a dummy option to distinguish initialization (no options)
%    from the empty set of options

:- dynamic display_option/1.

%  Database of all options
%  The database of the default options is in file "config.pro"

all_display([
  antecedent, assignment, backtrack, clause, conflict, decision,
  dominator, dot, dot_inc, evaluate, graph, incremental, label,
  learned, literal, none, partial, resolvent, result, skipped, sorted,
  tree, tree_inc, uip, unit, variable]).


%  init_display/0
%  If there are already display options, don't change them
%  Otherwise, set the default options

init_display :-
  display_copyright_notice,
  init_display1.

init_display1 :-
  display_option(_), !.
init_display1 :-
  default_display(List),
  set_display(List).


%  check_display/1
%    Check if a display option is set, otherwise fail

check_option(Option) :-
  display_option(Option).


%  set_display/1, clear_display/1
%    Set or clear display options
%    The argument can be "all", "default", a single options or
%      a list of options
%    For "all", "default" or a single option, recurse with a list
%    For each option call set_display1/1, clear_display1/1
%  set_display1/1, clear_display1
%    Set or clear an individual option

set_display(all) :- !,
  all_display(List),
  set_display(List).

set_display(default) :- !,
  clear_display(all),
  default_display(List),
  set_display(List).

%  Default can also be (the first) element in a list of options
set_display([default | Tail]) :- !,
  set_display(default),
  set_display(Tail).

set_display([Head | Tail]) :- !,
  set_display1(Head),
  set_display(Tail).

set_display([]) :- !.

set_display(Option) :-
  set_display([Option]), !.

set_display(X) :-
  not_recognized(X).

%  Already set, don't do anything
set_display1(Option) :-
  display_option(Option), !.

%  Otherwise, assert it after checking that it exists in all_display
set_display1(Option) :-
  all_display(List),
  member(Option, List), !,
  assert(display_option(Option)).

%  Otherwise, report that the option not recognized
set_display1(Option) :-
  not_recognized(Option).


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


%  If retract succeeds, the option is cleared
clear_display1(Option) :-
  retract(display_option(Option)), !.


%  If retract doesn't succeed, check that the option
%    exists in all_display
clear_display1(Option) :-
  all_display(List),
  member(Option, List), !.


%  Otherwise, report that the option not recognized
clear_display1(Option) :-
  not_recognized(Option).


%  not_recognized/1
%    Print an error message if the option is not recognized

not_recognized(X) :-
  write('Display option "'), write(X), write('" not recognized\n'),
  write('Run "usage" for a list of options\n').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Display configuration and usage

%  show_config/0
%    Display version, default mode, learn mode and display options
%      and current mode, learn mode and display options

show_config :-
  display_copyright_notice,
  default_alg_mode(M1),
  write('Default mode: '),
  write(M1), nl,
  default_learn_mode(M2),
  write('Default learn mode: '),
  write(M2), nl,
  default_display(D1),
  write('Default display options:\n'),
  sort(D1, D2),
  write(D2), nl,
  alg_mode(M3),
  write('Current mode: '),
  write(M3), nl,
  learn_mode(M4),
  write('Current learn mode: '),
  write(M4), nl,
  findall(D, display_option(D), List),
  write('Current display options:\n'),
  sort(List, List1),
  write(List1), nl,
  write('Variable order:'),
  get_order(Order),
  write_order(Order).
show_config.


%  write_order/1
%    Write the order of the variables if not the default lexicographic

write_order(default) :- !,
  write(' default').
write_order(Order) :-
  nl, write(Order).


%  usage/0 - print usage documentation
%  display_copyright_notice/0 - also used by dpll

display_copyright_notice :-
  write('LearnSAT v'),   version(V), write(V),
  write('. Copyright '), years(Y),   write(Y),
  write(' by Moti Ben-Ari. GNU GPL.\n').

usage :-
  display_copyright_notice, nl,
  write('dpll(Clauses, Decisions)\n'),
  write('  Clauses:   a list of list of literals (p or ~p)\n'),
  write('  Decisions: satisfying assignments or [] if unsatisfiable\n\n'),
  write('show_config\n'),
  write('  Show version, default and current mode and display options\n\n'),
  write('set_mode(Mode)\n'),
  write('  dpll: DPLL algorithm (default)\n'),
  write('  cdcl: DPLL with conflict-directed clause learning\n'),
  write('  ncb:  DPLL with CDCL and non-chronological backtracking\n\n'),
  write('set_learn_mode(Mode)\n'),
  write('  resolution: Learn clauses by resolution (default)\n'),
  write('  dominator:  Learn clauses from dominator\n\n'),
  write('set_order(Order)\n'),
  write('  default: variables assigned in lexicographical order\n'),
  write('  [...]:   variables assigned in the order [...]\n\n'),
  write('set_display(D), clear_display(D), where D can be a list\n'),
  write('  all          all the display options\n'),
  write('  default      default display options (marked *)\n\n'),
  write('  antecedent   antecedents of the implied literals\n'),
  write('  assignment   assignments that caused a conflict\n'),
  write('  backtrack *  level of non-chronological backtracking\n'),
  write('  clause       clauses to be checked for satisfiability\n'),
  write('  conflict *   conflict clauses\n'),
  write('  decision *   decision assignments\n'),
  write('  dominator    learned clause from dominator\n'),
  write('  dot          implication graphs (final) in dot format\n'),
  write('  dot_inc      implication graphs (incremental) in dot format\n'),
  write('  evaluate     evaluation of clauses for an assignment\n'),
  write('  graph        implication graphs (final) in textual format\n'),
  write('  incremental  implication graphs (incremental) in textual format\n'),
  write('  label        graphs and trees labeled with clauses\n'),
  write('  learned *    learned clause by resolution\n'),
  write('  literal      literals found assigned during CDCL\n'),
  write('  partial      partial assignments so far\n'),
  write('  resolvent *  resolvents created during CDCL\n'),
  write('  result *     result of the algorithm with statistics\n'),
  write('  skipped *    assignments skipped when backtracking\n'),
  write('  sorted *     assignments displayed in sorted order\n'),
  write('  tree         trees of assignments (final) in dot format\n'),
  write('  tree_inc     trees of assignments (incremental) in dot format\n'),
  write('  uip *        unique implication points\n'),
  write('  unit *       unit clauses\n'),
  write('  variable     variables that are not assigned so far\n').
