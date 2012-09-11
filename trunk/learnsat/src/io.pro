% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(io, [
  write_assignment/1, write_assignment/2, write_assignments/1,
  write_clause/2, write_clauses/2,
  write_dot/2, write_paths/1, write_graph/2
  ]).

:- use_module([counters, modes, config]).
  
%  IO predicates for displaying clauses and assignments
%    and for displaying implications graphs and writing to a dot file 

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
  (N == 3 ->
    nl, write(' '), N1 is 0 ;
    N1 is N + 1),
  write_assignments1(T, N1).


%  write_assignment/1
%    Write the assignment as Variable=Value@Depth/Antecedent
%    For an implication graph, the assignment can be 'kappa'
%  write_assignment/2
%    Write the antecedent only if second argument is 'yes'
%    Used when writing the dot graph
%  write_level/1
%    Only write level and antecedent if not dpll mode
%  write_antecedent/2
%    Only write antecedent if display option set

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
  write('/'),
  (Unit = yes -> write('nil') ; write(Unit)).
write_antecedent(_, _).


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

%  write_arrow/2 - write the arrows with a number or the clause itself

write_arrow(edge(From, N, To), Clauses) :-
   write_assignment(From, no),
   write(' --'),
  (Clauses = [] ->
    write(N) ;
    nth1(N, Clauses, C), write(N), write('.'), write(C)    
  ),
  write('--> '),
  write_assignment(To, no).


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


%  write_dot/1
%    Write the implication graph to a file in dot format for GraphViz
%    Only the edges are needed as dot takes the nodes from them
%    Call write_assignment to write each node (including "kappa")
%    Label each edge with its clauses or just its number if Clauses = []
%    The prologue for the dot file is taken from "config.pro"

write_dot(graph(_, Edges), Clauses) :-
  create_file_name(Name),
  tell(Name),
  dot_prologue(D),
  write(D),
  write_dot1(Edges, Clauses),
  write('}'),
  told.

write_dot1([], _).
write_dot1([edge(From, N, To) | Tail], Clauses) :-
  write('"'),
  write_assignment(From, no),
  write('"  ->  "'),
  write_assignment(To, no),
  write('"'),
  write('  [label="'),
  (Clauses = [] ->
    write(N) ;
    nth1(N, Clauses, C), write(N), write('. '), write(C)    
  ),
  write('"];\n'),
  decorate_decision_node(From),
  write_dot1(Tail, Clauses).


%  create_file_name/1
%    The File name is taken from the file argument of the program
%      remove the extension from the name and add "-NN.dot"

create_file_name(Name) :-
  get_file_counter(F1),
  current_prolog_flag(argv, [_, File | _]),
  remove_extension(File, F2),
  (F1 < 10 -> atom_concat('-0', F1, F1a); atom_concat('-', F1, F1a)),
  atom_concat(F2, F1a, F3),
  atom_concat(F3, '.dot', Name),
  increment(file).


%  remove_extension/2, remove_period/2
%    Remove the extension by search for the period from the end

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


%  decorate_decision_node/1
%    Decorate a decision node
%    The decoration is defined in "config.pro"

decorate_decision_node(Node) :-
  Node = assign(_, _, _, yes), !,
  write('"'),
  write_assignment(Node, no),
  write('"'),
  dot_decorate(D),
  write(D).
decorate_decision_node(_).
