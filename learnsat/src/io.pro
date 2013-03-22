% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(io, [
  write_assignment/1, write_assignment/2, write_assignments/1,
  write_clause/2, write_clauses/2, write_arrow_label/2,
  write_paths/1, write_graph/2
  ]).

:- use_module([counters, modes, config]).
  
%  IO predicates for displaying clauses and assignments
%    and for displaying implication and dominator graphs 

%  write_clauses/2
%    Each clause is written on a separate line with a number
%      Clauses - the set of clauses to write
%      Clauses (second argument) - to obtain clause number

write_clauses([], _) :- !,
  write('[]').
write_clauses(A, Clauses) :-
  write('[\n'),
  write_clauses1(A, Clauses),
  write(']').

write_clauses1([H], Clauses) :- !,
  write_clause(H, Clauses), nl.
write_clauses1([H|T], Clauses) :-
  write_clause(H, Clauses),
  write(',\n'),
  write_clauses1(T, Clauses).

write_clause(C, Clauses) :-
  nth1(N, Clauses, C),
  write(N), write('. '), write(C).


%  write_assignments/1
%    Sort and then write a list of assignments using write_assignment/1
%      Assignments - a list of assignments
%  write_assignments1/1
%    Auxiliary predicate; write three assignments per line

write_assignments([]) :- !,
  write('[]').
write_assignments(A) :-
  sort(A, A1),
  write('['),
  write_assignments1(A1, 0),
  write(']').

write_assignments1([H], _) :- !,
  write_assignment(H).
write_assignments1([H|T], N) :-
  write_assignment(H),
  write(','),
  break_line(N, N1),
  write_assignments1(T, N1).

break_line(3, 0) :- !,
  write('\n ').
break_line(N, N1) :- N1 is N + 1.

%  write_assignment/1
%    Write the assignment as Variable=Value@Depth/Antecedent
%    For an implication graph, the assignment can be 'kappa'
%  write_assignment/2
%    Called by write_assignment/1 (second argument is yes),
%      or directly with second argument no
%    Yes or no: write the antecedent or not
%  write_level/1
%    Write level and antecedent only if not dpll mode
%  write_antecedent/2
%    Write antecedent only if display option set

write_assignment(A) :-
  write_assignment(A, yes).

write_assignment(A, Antecedent) :-
  A = assign(V, N, _, _), 
  write(V), write('='), write(N),
  write_level(A),
  write_antecedent(A, Antecedent).
write_assignment(kappa, _) :-
  write(kappa).

write_level(assign(_, _, Depth, _)) :- 
  get_mode(Mode), 
  Mode \= dpll, !,
  write('@'),
  write(Depth).
write_level(_).

write_antecedent(assign(_, _, _, Unit), yes) :-
  get_mode(Mode),
  Mode \= dpll,
  check_option(antecedent), !,
  write_antecedent1(Unit).
write_antecedent(_, _).

write_antecedent1(yes) :- !.
write_antecedent1(Unit) :-
  write('/'),
  write(Unit).

%  write_graph/2
%    graph(Nodes, Edges) - where Nodes and Edges are lists
%    Clauses - [] means just write the number on the edge
%    Clauses - non-empty list means write the clause itself

write_graph(graph(Nodes, Edges), Clauses) :-
  write('[\n'),
  write_nodes(Nodes),
  write('\n]\n[\n'),
  write_edges(Edges, Clauses),
  write('\n]').

%  write_nodes/1 - write the list of nodes as assignments

write_nodes([]).
write_nodes([N]) :- !,
  write_assignment(N, no).
write_nodes([N | Tail]) :-
  write_assignment(N, no),
  write(',\n'),
  write_nodes(Tail).

%  write_edges/2 - write the list of edges

write_edges([], _).
write_edges([E], Clauses) :- !,
  write_arrow(E, Clauses).
write_edges([E | Tail], Clauses) :-
  write_arrow(E, Clauses),
  write(',\n'),
  write_edges(Tail, Clauses).

%  write_arrow/2 - write the arrows with a number and optionally
%  write_arrow_label/2 - write the clause, not just the number

write_arrow(edge(From, N, To), Clauses) :-
  write_assignment(From, no),
  write(' --'),
  write(N),
  write_arrow_label(N, Clauses),
  write('--> '),
  write_assignment(To, no).

write_arrow_label(_, []) :- !.
write_arrow_label(N, Clauses) :-
  nth1(N, Clauses, C),
  write('.'),
  write(C).    


%  write_paths/1
%    Write all paths from the decision node to kappa
%    Used when searching for a dominator
%      Paths - a list of paths

write_paths([]).
write_paths([Head | Tail]) :-
  write_one_path(Head),
  write_paths(Tail).

%  write_one_path/1
%    Write one path as node -> node -> ...

write_one_path([kappa]) :- !,
  write('kappa\n').
write_one_path([Assignment | Tail]) :-
  write_assignment(Assignment),
  write(' --> '),
  write_one_path(Tail).
