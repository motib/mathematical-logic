% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(counters,
  [show_counters/2, increment/1, init_counters/0, get_file_counter/1]).

%  Counters for:
%    units propagated, choices made, conflicts encountered,
%    generating file names for implication graphs

:- dynamic
     unit_counter/1,  choice_counter/1, conflict_counter/1,
     file_counter/1.

%  init_counters - set counters to 0

init_counters :-
  retractall(unit_counter(_)),
  assert(unit_counter(0)),
  retractall(choice_counter(_)),
  assert(choice_counter(0)),
  retractall(conflict_counter(_)),
  assert(conflict_counter(0)),
  retractall(file_counter(_)),
  assert(file_counter(0)).


%  increment/1 - a counter

increment(unit) :-
  retract(unit_counter(N)),
  N1 is N + 1,
  assert(unit_counter(N1)).
increment(choice) :-
  retract(choice_counter(N)),
  N1 is N + 1,
  assert(choice_counter(N1)).
increment(conflict) :-
  retract(conflict_counter(N)),
  N1 is N + 1,
  assert(conflict_counter(N1)).
increment(file) :-
  retract(file_counter(N)),
  N1 is N + 1,
  assert(file_counter(N1)).


%  show_counters/2    - write the counters
  
show_counters(Clause_Count, Variable_Count) :-
  write('Statistics: clauses='),
  write(Clause_Count),
  write(', variables='),
  write(Variable_Count),
  unit_counter(Unit_Count), 
  write(', units='),
  write(Unit_Count),
  choice_counter(Choice_Count),
  write(', choices='),
  write(Choice_Count),
  conflict_counter(Conflict_Count),
  write(', conflicts='),
  write(Conflict_Count), nl.


%  get_file_counter/1 - return the file counter

get_file_counter(N) :-
  file_counter(N).
