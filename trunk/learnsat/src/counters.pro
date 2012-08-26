% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(counters,
  [show_counters/0, increment/1, init_counters/2]).

:- dynamic unit_counter/1, choice_counter/1, conflict_counter/1,
   clause_counter/1, variable_counter/1,
   file_counter/1.

%  Counters for units propagated, choices made, conflicts encountered
%    init_counters/2 - set clause and variable counters and
%                      reset the other counters to 0
%    increment/1     - increment unit, choice or conflict counter
%    show_counters/0 - write the counters
%
%  Counter file_counter used in this module for generating
%    file names for dot files of the implication graphs

init_counters(Clauses, Variables) :-
  retractall(clause_counter(_)),
  length(Clauses, C),
  assert(clause_counter(C)),
  retractall(variable_counter(_)),
  length(Variables, V),
  assert(variable_counter(V)),
  retractall(unit_counter(_)),
  assert(unit_counter(0)),
  retractall(choice_counter(_)),
  assert(choice_counter(0)),
  retractall(conflict_counter(_)),
  assert(conflict_counter(0)),
  retractall(file_counter(_)),
  assert(file_counter(1)).


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


show_counters :-
  clause_counter(N1),
  variable_counter(N2),
  unit_counter(N3), 
  choice_counter(N4),
  conflict_counter(N5),
  write('Clauses='),
  write(N1),
  write(', variables='),
  write(N2),
  write(', units propagated='),
  write(N3),
  write(', choices='),
  write(N4),
  write(', conflicts='),
  write(N5), nl.