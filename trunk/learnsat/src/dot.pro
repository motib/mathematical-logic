% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Generation of dot files for implications graphs and semantic trees

:- module(do, [write_dot/2, write_tree/2, init_tree/0]).

%  Prologues and decorations for the dot files are in config.pro

:- use_module([counters, io, config, modes]).

%  Generate the semantic tree of assignments
%    The tree is actually a DAG and the nodes are assignments
%    rather than the usual variable names

% The node database contains the final assignment that determines
%   if the assignments lead to a conflict or satisfy the formula
% An edge contains two assignments: source and target

:- dynamic edge/2, node/2.

% init_tree/0
%   Initialize by retract existing databases
init_tree :-
  retractall(edge(_, _)),
  retractall(node(_, _)).

% write_tree/2
%   Assignments so far
%     The assignments are in reverse order (last assignment is first)
%   Result - conflict or sat for decorating nodes
write_tree(Assignments, Result) :-
    % Create file name, tell and write dot prologue
  create_file_name(st, Name),
  tell(Name),
  dot_prologue(tb, D),
  write(D),
    % Assert the last assignment if a conflict or satisfiable
  assert_node(Assignments, Result),
    % Assert all the edges as pairs of assignments
  reverse(Assignments, Assignments1),
  assert_edges(root, Assignments1),
    % Get all the edges and write the tree
  get_edges(Edges),
  write_tree(Edges),
  write('}'),
  told.


%  assert_node/2
%      Assignments - assignments to conflict or satisfiable
%      Result - conflict or sat
%    The first element of the list is the last assignment
%    Assert it if conflict or satisfiable
assert_node([Head | _], conflict) :-
  \+node(Head, conflict), !,
  assertz(node(Head, conflict)).
assert_node([Head | _], sat) :-
  \+node(Head, sat), !,
  assertz(node(Head, sat)).
assert_node(_, _).


%  assert_edges/2
%      Previous - the previous assignment is the source of the edge
%      Assignments - the rest of the assignments
%    Assert the edge if it does not exist and recurse
assert_edges(_, []).
assert_edges(Previous, [Head | Tail]) :-
  \+edge(Previous, Head), !,
  assertz(edge(Previous, Head)),
  assert_edges(Head, Tail).
assert_edges(_, [Head | Tail]) :-
  assert_edges(Head, Tail).

%  get_edges/1
%    Return the set of edges
%    (Unlike findall, setof can fail so a predicate is necessary)
  
get_edges(Edges) :-
  setof(edge(Source,Target), edge(Source,Target), Edges), !.
get_edges([]).


%  write_tree/1
%      Edges - the set of edges
%    Start from an extra "root" node

write_tree([]).
write_tree([edge(Source,Target) | Tail]) :-
  write_node(Target),
  write('"'), write(Source), write('"'),
  write(' -> '),
  write('"'), write(Target), write('"'),
  write(';\n'),
  write_tree(Tail).


%  write_node/1

write_node(root) :- !.
write_node(Node) :-
  Node = assign(Variable, Value, Level, _),
  write('"'), write(Node), write('"'),
  write('  [label="'),
  write(Variable), write('='),
  write(Value), 
  write_level(Level),
  write_reason(Node),
  write('"] '),
  decorate_node(Node),
  write(';\n').


%  write_level/1
%    The level is not written for dpll mode

write_level(Level) :-
  get_mode(Mode),
  Mode \= dpll, !,
  write('/'),
  write(Level).
write_level(_).


%  write_reason/2
%    For propagated nodes, add the antecedent clause to the label

write_reason(Node) :-
  arg(4, Node, yes), !.
write_reason(Node) :-
  check_option(label),
  arg(4, Node, Antecedent), !,
  write('\\n'),
  write(Antecedent).
write_reason(_).


%  decorate_node/1
%    Decorate decision, conflict and sat nodes
%    Make sure that sat is before conflict
%      so that a node which is both is marked sat

decorate_node(Node) :-
  node(Node, sat), !,
  dot_decorate(sat, D),
  write(D).
decorate_node(Node) :-
  node(Node, conflict), !,
  dot_decorate(conflict, D),
  write(D).
decorate_node(Node) :-
  arg(4, Node, yes), !,
  dot_decorate(decision, D),
  write(D).
decorate_node(_).


%  write_dot/2
%      Graph - the graph to be written
%      Clauses - edges are labeled by clause numbers but the antecedent
%                clauses can be displayed instead
%                this variable contains the set of clauses
%    Write the implication graph to a file in dot format for GraphViz
%    Only the edges are needed as dot takes the nodes from them
%    Call write_assignment to write each node (including kappa)

write_dot(graph(_, Edges), Clauses) :-
  create_file_name(ig, Name),
  tell(Name),
  dot_prologue(lr, D),
  write(D),
  write_dot1(Edges, Clauses),
  write('}'),
  told.

%  write_dot1/2
%      Graph - the graph to be written
%      Clauses - list of clauses
%    Write each edge from Graph
%      labeled with the number or name of the antecedent clause

write_dot1([], _).
write_dot1([edge(From, N, To) | Tail], Clauses) :-
  write('"'),
  write_assignment(From, no),
  write('"  ->  "'),
  write_assignment(To, no),
  write('"'),
  write('  [label="'),
  write(N),
  write_arrow_label(N, Clauses),
  write('"];\n'),
  decorate_decision_node(From),
  write_dot1(Tail, Clauses).


%  decorate_decision_node/1
%    Decorate a decision node of the implication graph

decorate_decision_node(Node) :-
  arg(4, Node, yes), !,
  write('"'),
  write_assignment(Node, no),
  write('"'),
  dot_decorate(decision, D),
  write(D),
  write(';\n').
decorate_decision_node(_).


%  create_file_name/2
%    Graph - ig for implication graph, st for semantic tree
%    The File name is taken from the file argument of the program
%      remove the extension from the name and add "-NN.dot"
%  NOTE: The use of current_prolog_flag to obtain the file name
%        may not be portable to non-SWI Prologs

create_file_name(Graph, Name1) :-
  get_file_counter(Graph, Counter),
  current_prolog_flag(argv, [_, File | _]),
  file_name_extension(Base, _, File),
  atom_concat(Base, '-', Base1),
  atom_concat(Base1, Graph, Base2),
  pad_file_number(Counter, Counter1),
  atom_concat(Base2, Counter1, Name),
  atom_concat(Name, '.dot', Name1),
  increment(Graph).

%  pad_file_number(F, F1)
%    If F is a single digit, pad on left with 0
pad_file_number(F, F1) :-
  F < 10, !,
  atom_concat('-0', F, F1).
pad_file_number(F, F1) :-
  atom_concat('-', F, F1).
