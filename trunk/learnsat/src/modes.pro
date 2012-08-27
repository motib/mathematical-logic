% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Set/clear display options and algorithm mode
%  Show the configuration
%  Show the "usage" message

:- module(modes, [
  set_display/1, clear_display/1, init_display/0, check_option/1,
  set_mode/1, get_mode/1, init_mode/1,
  usage/0, display_copyright_notice/0,
  show_config/0]).

:- use_module([config,io]).


%  LearnSAT can be run in three modes;
%    dpll - DPLL algorithm
%    cdcl -   DPLL with conflict-directed clause learning (CDCL)
%    ncb  -   DPLL with CDCL and non-chronological backtracking (NCB)

:- dynamic mode/1.

%  init_mode/01 set_mode/1, get_mode/1
%    Mode - dpll (default) or cdcl or ncb

%  If there is already a mode, don't change it and return it
%    Otherwise, set the default mode
init_mode(Mode) :-
  mode(Mode), !.
init_mode(Mode) :-
  default_mode(Mode),
  assert(mode(Mode)).

%  set_mode/1 - Set a new mode, checking that it is a legal one

set_mode(Mode) :-
  retractall(mode(_)),
  check_mode(Mode),
  assert(mode(Mode)).

check_mode(dpll).
check_mode(cdcl).
check_mode(ncb).
check_mode(X) :-
  write('Mode '), write(X), write(' not recognized.\n'),
  write('Run "usage" for a list of modes.\n').

%  get_mode/1 - Get the mode, or return default. 

get_mode(Mode) :-
  mode(Mode), !.
get_mode(Mode) :-
  default_mode(Mode).

  
%  Database of display options
%    "none" is a dummy option to distinguish initialization from none

:- dynamic display_option/1.

%  check_display/1 - check if a display option is set
check_option(Option) :-
  display_option(Option).


%  all_display/1, default_display/1 (in file config), init_display/0

all_display([
  assignments, backtrack, clauses, conflict, decision, dot, graph,
  incremental, learned, literal, none, resolvent, result, skipping, uip,
  unit, variables]).

%  If there are already display options, don't change them
init_display :-
  display_copyright_notice,
  init_display1.

init_display1 :-
  display_option(_), !.
init_display1 :-
  default_display(List),
  set_display(List).


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


%  non_recognized/1
%    Print an error message if the option is not recognized

not_recognized(X) :-
  write('Display option '), write(X), write(' not recognized.\n'),
  write('Run "usage" for a list of options.\n').


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
  get_mode(M2),
  write('Current mode: '),
  write(M2), nl,
  findall(D, display_option(D), List),
  write('Current display options:\n'),
  write(List), nl.
show_config.


%  usage/0 - print usage documentation
%  display_copyright_notice/0 - also used by dpll

display_copyright_notice :-
  write('LearnSAT v'),
  version(V),
  write(V),
  write('. Copyright 2012 by Moti Ben-Ari. GNU GPL.\n').

usage :-
  display_copyright_notice, nl,
  write('  dpll(Clauses, Decisions)\n'),
  write('    Clauses:   a list of list of literals (p or ~p)\n'),
  write('    Decisions: satisfying assignments or [] if unsatisfiable\n\n'),
  write('  show_config,\n'),
  write('    Show version, default and current mode and display options\n'),
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
