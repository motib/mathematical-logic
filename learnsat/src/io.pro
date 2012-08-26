% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  IO predicates for displaying clauses and assignments
%    and for displaying implications graphs and writing to a dot file 

:- module(io, [
  write_assignment/1, write_assignments/1,
  write_clause/2, write_clauses/2,
  write_dot/1, write_graph/1
  ]).

%  write_clauses/2
%    Clauses - write the set of clauses
%    Clauses (second occurrence) - to obtain clause number
%      Each clause on a separate line with a number

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
%    Assignments - a list of assignments
%  write_assignments1/1
%    Auxiliary predicate
%  write_assignment/1
%    Assignment - write the assignment as Variable=Value@Depth
%    For an implication graph, the assignment can be "kappa"

%  Sort and then write a list of assignments using write_assignment/1
write_assignments([]) :- !,
  write('[]').
write_assignments(A) :-
  sort(A, A1),
  write('['),
  write_assignments1(A1),
  write(']').

write_assignments1([H]) :- !,
  write_assignment(H).
write_assignments1([H|T]) :-
  write_assignment(H),
  write(','),
  write_assignments1(T).

write_assignment(assign(V, N, Depth, _)) :- !,
  write(V), write('='), write(N), write('@'), write(Depth).
write_assignment(kappa) :-
  write(kappa).


%  write_graph/1
%    graph(Nodes, Edges)

write_graph(graph(Nodes, Edges)) :-
  write('[\n'),
  write_nodes(Nodes),
  write('\n]\n[\n'),
  write_edges(Edges),
  write('\n]').

write_nodes([]).
write_nodes([node(N)]) :- !,
  write_assignment(N).
write_nodes([node(N) | Tail]) :-
  write_assignment(N),
  write(',\n'),
  write_nodes(Tail).

write_edges([]).
write_edges([edge(From, N, To)]) :- !,
  write_assignment(From), write('-'),
  write(N), write('->'), write_assignment(To).
write_edges([edge(From, N, To) | Tail]) :-
  write_assignment(From), write('-'), 
  write(N), write('->'), write_assignment(To), write(',\n'),
  write_edges(Tail).


%  write_dot/1
%    Write the implication Graph to a file in dot format for GraphViz
%  create_file_name/1
%    The File name is taken from the file argument of the program
%  get_file_counter/1
%    N is taken from file_counter (or 0 if not initialized)
%      and is incremented for each subsequent file
%  remove_extension/2, remove_period/2
%    Remove the extension from the file name and add "-graph-N.dot"
%
%  write_dot/1
%    Edges - only the edges are needed as dot takes the nodes from them
%    Call write_assignment to write each node (including "kappa")
%    Label each edge with the clause
%  label_decision_node/1
%    Decorate a decision node

write_dot(graph(_, Edges)) :-
  create_file_name(Name),
  tell(Name),
  write('digraph G {\n  rankdir=LR;\n'),
  write_dot1(Edges),
  write('}'),
  told.

create_file_name(Name) :-
  get_file_counter(F1),
  current_prolog_flag(argv, [_, File | _]),
  remove_extension(File, File1),
  atom_concat(File1, '-graph-', F2),
  atom_concat(F2, F1, F3),
  atom_concat(F3, '.dot', Name),
  F4 is F1 + 1,
  assert(file_counter(F4)).

get_file_counter(N) :- 
  retract(file_counter(N)), !.
get_file_counter(0).

remove_extension(File, File1) :-
  atom_chars(File, List),
  reverse(List, List1),
  remove_period(List1, List2), !,
  reverse(List2, List3),
  atom_chars(File1, List3).
remove_extension(File, File).

remove_period(['.' | Tail], Tail) :- !.
remove_period([_ | Tail], Tail1)  :-
  remove_period(Tail, Tail1).

write_dot1([]).
write_dot1([edge(From, N, To) | Tail]) :-
  write('"'),
  write_assignment(From),
  write('"  ->  "'),
  write_assignment(To),
  write('"'),
  write('  [label="'),
  write(N),
  write('"];\n'),
  label_decision_node(From),
  write_dot1(Tail).

label_decision_node(Node) :-
  Node = assign(_, _, _, yes), !,
  write('"'),
  write_assignment(Node),
  write('"'),
  write(' [style="bold" color="red"];\n').
label_decision_node(_).
