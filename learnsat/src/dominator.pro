% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Export main predicate dpll/2
:- module(dominator, [compute_learned_clause_by_dominator/3]).

%  Modules directly used by dpll
:- use_module([auxpred,modes,display]).

%  Compute a learned clause by locating a dominator
%  This is used only for display and the result is ignored

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  compute_learned_clause_by_dominator/3
%    Graph - the implication graph
%    Level - the highest level in the graph
%    Dominator - return the dominator for emphasis on dot graph

compute_learned_clause_by_dominator(graph(Nodes, Edges), Level, Dominator) :-
      % Run only if the display option dominator is set
  check_option(dominator), !,
      % From the decision assignment at the highest level
  N = assign(_, _, Level, yes),
  member(N, Nodes),
      % Find all paths to the node kappa
  findall(Path, get_path(Edges, N, kappa, [N], Path), Path_List),
      % Find a dominator for the list of paths
  get_dominator(Path_List, N, Dominator),
      % Find all decision assignments of lower level in the graph
  findall(D, lower_decision_assignment(D, Nodes, Level), Decisions),
      % Of them, select those with no path to the dominator
  findall(D1, no_path(D1, Decisions, Edges, Dominator), Result),
      % The learned clause is built from these decision assignments
      % together with the dominator
  union(Result, [Dominator], Learned),
      % Convert to clause form and complement each literal
  to_complemented_clause(Learned, Clause),
  display(dominator, Path_List, Dominator, Decisions, Result, Clause).
compute_learned_clause_by_dominator(_, _, no).


%  get_path/5
%    Get a path in the implication graph
%    Since the graph is a dag, a transitive computation suffices
%      Edges  - the edges of the graph
%      Source - the source node
%      Target - the target node
%      So_Far - the path so far
%      Path   - the path that is retured

%  When the Source equals the target the path has been found
%  Reverse the list so that the source comes first
get_path(_, Target, Target, So_Far, Path) :-
  reverse(So_Far, Path).
get_path(Edges, Source, Target, So_Far, Path) :-
  member(edge(Source, _, Next), Edges),
  get_path(Edges, Next, Target, [Next | So_Far], Path).


%  get_dominator/3, get_dominator1/3
%    Dominators are nodes which appear in all paths
%    But, a dominator is not the Source decision assignment
%      or the kappa Target
%    Take the intersection of all the path lists
%      Path_List  - the list of paths
%      Source     - the source node
%      Dominator  - the dominator that is returned

get_dominator([Head | Tail], Source, Dominator) :-
  get_dominator1(Tail, Head, Result),
  subtract(Result, [Source, kappa], [Dominator | _]).

get_dominator1([Head | Tail], So_Far, Result) :-
  intersection(Head, So_Far, So_Far1),
  get_dominator1(Tail, So_Far1, Result).
get_dominator1([], So_Far, So_Far).


%  lower_decision_assignment/3
%    Find and return a decision assignment of lower level than Level
%      A     - decision assignment returned
%      Nodes - nodes of the graph
%      Level - highest level of assignment in the graph

lower_decision_assignment(A, Nodes, Level) :-
  member(A, Nodes),
  A = assign(_, _, L, yes),
  L < Level.


%  no_path/4
%    Succeed if there is no path from a node to the dominator
%      A         - a decision assignment returned
%      Decisions - the set of decision assignments 
%      Edges     - the edges of the graph
%      Dominator - the dominator

no_path(A, Decisions, Edges, Dominator) :-
  member(A, Decisions),
  \+ get_path(Edges, A, Dominator, [], _).
