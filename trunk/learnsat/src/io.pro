% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  IO predicates for displaying clauses and assignments
%    and for displaying implications graphs and writing to a dot file 

:- module(io, [
  write_assignment/1, write_assignments/1,
  write_clause/2, write_clauses/2,
  write_dot/2, write_graph/1
  ]).

:- use_module([counters, modes]).
  
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

write_assignment(assign(V, N, Depth, _)) :-
  write(V), write('='), write(N),
  get_mode(Mode),
  (Mode \= dpll -> write('@'), write(Depth) ; true).
write_assignment(kappa) :-
  write(kappa).


%  write_graph/1
%    graph(Nodes, Edges) where Nodes and Edges are lists

write_graph(graph(Nodes, Edges)) :-
  write('[\n'),
  write_nodes(Nodes),
  write('\n]\n[\n'),
  write_edges(Edges),
  write('\n]').

%  write_nodes/1 - write the list of nodes as assignments

write_nodes([]).
write_nodes([node(N)]) :- !,
  write_assignment(N).
write_nodes([node(N) | Tail]) :-
  write_assignment(N),
  write(',\n'),
  write_nodes(Tail).

%  write_edges/1 - write the list of edges as "source -n-> target"

write_edges([]).
write_edges([edge(From, N, To)]) :- !,
  write_assignment(From), write(' -'),
  write(N), write('-> '), write_assignment(To).
write_edges([edge(From, N, To) | Tail]) :-
  write_assignment(From), write(' -'), 
  write(N), write('-> '), write_assignment(To), write(',\n'),
  write_edges(Tail).


%  write_dot/1
%    Write the implication graph to a file in dot format for GraphViz
%    Only the edges are needed as dot takes the nodes from them
%    Call write_assignment to write each node (including "kappa")
%    Label each edge with its clauses or just its number if Clauses = []

write_dot(graph(_, Edges), Clauses) :-
  create_file_name(Name),
  tell(Name),
  write('digraph G {\n  rankdir=LR;\n'),
  write_dot1(Edges, Clauses),
  write('}'),
  told.

write_dot1([], _).
write_dot1([edge(From, N, To) | Tail], Clauses) :-
  write('"'),
  write_assignment(From),
  write('"  ->  "'),
  write_assignment(To),
  write('"'),
  write('  [label="'),
  (Clauses = [] ->
    write(N) ;
    nth1(N, Clauses, C), write(N), write('. '), write(C)    
  ),
  write('"];\n'),
  label_decision_node(From),
  write_dot1(Tail, Clauses).


%  create_file_name/1
%    The File name is taken from the file argument of the program
%      remove the extension from the name and add "-graph-N.dot"

create_file_name(Name) :-
  get_file_counter(F1),
  current_prolog_flag(argv, [_, File | _]),
  remove_extension(File, File1),
  atom_concat(File1, '-graph-', F2),
  (F1 < 10 -> atom_concat('0', F1, F1a); F1a = F1),
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

%  label_decision_node/1
%    Decorate a decision node with bold and red

label_decision_node(Node) :-
  Node = assign(_, _, _, yes), !,
  write('"'),
  write_assignment(Node),
  write('"'),
  write(' [style="bold" color="red"];\n').
label_decision_node(_).
