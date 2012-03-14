% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Systematic construction of semantic tableaux for temporal logic.

:- dynamic num/1.      % Counter for nodes numbers.
:- dynamic tau/2.      % Database of transitions.

%  create(Fml, Tab) -
%    Tab is the tabelau for the formula Fml.
%    Also returns:
%      list of states,
%      list of transitions,
%      tableau result (open/closed/need to check fulfilment)
%      SCC nodes and edges
%      Results of fulfilment check
%
%  t(Fmls, Left, Right, N, Path)
%     Fmls is a list of formula at the root of this tableau and
%     Left and Right are the subtrees (Right ignored for alpha rule).
%     N is the number of the node.
%     Path is the path to the ancestor nodes.
%
%     The tableau is constructed by instantiating the logical
%     variables for the subtrees.
%     After construction, check if it is open or closed.
%     If neither, create the states and check fulfilment.

create_tableau(Fml, Tab,
               States, Tau, Tab_Result,
               SCCs, Edges, Fulfil_Result) :-
  retractall(num(_)), assert(num(1)),
  get_num(N),
  Tab = t([Fml], _, _, N, []),
  extend_tableau(Tab),
  check_tableau(Tab, Tab_Result),
  create_states(Tab, [], States, Tau),
  check_fulfilment(Tau, SCCs, Edges, States, Fulfil_Result).

%  extend_tableau(t(Fmls, Left, Right, N, Path)) -
%    Perform one tableau rule.
%      1. Check for a pair of contradicatory formulas in Fmls (closed).
%      2. Check if Fmls contains only literals (open).
%      3. Perform an alpha or beta rule.
%      4. Extend state path.

extend_tableau(t(Fmls, closed, empty, _, _)) :- 
  check_closed(Fmls), !.

extend_tableau(t(Fmls, open,   empty, _, _)) :-
  contains_only_literals(Fmls), !.

extend_tableau(t(Fmls, Left,   empty, N, Path)) :-
  alpha_rule(Fmls, Fmls1), !,
  get_num(N1),
  Left = t(Fmls1, _, _, N1, [pt(Fmls,N)|Path]),
  extend_tableau(Left).

extend_tableau(t(Fmls, Left,   Right, N, Path)) :-
  beta_rule(Fmls, Fmls1, Fmls2), !,
  get_num(N1),
  get_num(N2),
  Left  = t(Fmls1, _, _, N1, [pt(Fmls,N)|Path]),
  Right = t(Fmls2, _, _, N2, [pt(Fmls,N)|Path]),
  extend_tableau(Left),
  extend_tableau(Right).

%  next rule: Search up path for an ancestor with the same formulas.

extend_tableau(t(Fmls, connect(N), empty, _, Path)) :-
  next_rule(Fmls, Fmls1),
  search(Path, Fmls1, N), !.

extend_tableau(t(Fmls, Left,   empty, N, Path)) :-
  next_rule(Fmls, Fmls1), !,
  get_num(N1),
  Left = t(Fmls1, _, _, N1, [pt(Fmls,N)|Path]),
  extend_tableau(Left).

%  check_closed(Fmls)
%    Fmls is closed if it contains contradictory formulas.
%  contains_only_literals(Fmls)
%    Traverse Fmls list checking that each is a literal.
%  literal(Fml)
%    Fml is a propositional letter or a negation of one.

check_closed(Fmls) :-
  member(F, Fmls),
  member(neg F, Fmls).

contains_only_literals([]).
contains_only_literals([Fml | Tail]) :-
  literal(Fml),
  contains_only_literals(Tail).

literal(Fml)     :- atom(Fml).
literal(neg Fml) :- atom(Fml).

%  alpha_rule(Fmls, Fmls1)
%    Fmls1 is Fmls with an alpha deleted and alpha1, alpha2 added.
%    Special clause for double negation.
%  beta_rule(Fmls, Fmls1, Fmls2)
%    Fmls1 (Fmls2) is Fmls with a beta deleted and beta1 (beta2) added.
%
%  alpha(A1 opr A2, A1, A2)
%  beta(A1 opr A2, A1, A2)
%    Database of rules for each operator.

alpha_rule(Fmls, Fmls2) :-
  member(A, Fmls),
  alpha(A, A1, A2), !,
  delete(Fmls, A, Fmls1),
  union(Fmls1, [A1, A2], Fmls2).
alpha_rule(Fmls, Fmls2) :-
  member(A, Fmls),
  A = neg neg A1,
  delete(Fmls, A, Fmls1),
  union(Fmls1, [A1], Fmls2).
  
beta_rule(Fmls, Fmls2, Fmls3) :-
  member(B, Fmls),
  beta(B, B1, B2),
  delete(Fmls, B, Fmls1),
  union(Fmls1, [B1], Fmls2),
  union(Fmls1, [B2], Fmls3).

alpha(A1 and A2, A1, A2).
alpha(neg (A1 imp A2), A1, neg A2).
alpha(neg (A1 or A2), neg A1, neg A2).
alpha(A1 eqv A2, A1 imp A2, A2 imp A1).
alpha(always A, A, next always A).
alpha(neg eventually A, neg A, neg next eventually A).
  
beta(B1 or B2, B1, B2).
beta(B1 imp B2, neg B1, B2).
beta(neg (B1 and B2), neg B1, neg B2).
beta(neg (B1 eqv B2),  neg (B1 imp B2), neg (B2 imp B1)).
beta(eventually A, A, next eventually A).
beta(neg always A, neg A, neg next always A).

%  next rule -
%    Remove next operator from all formulas for next node.
%    Ignore other (atomic) formulas.

next_rule([], []).
next_rule([next A | Tail], [A | Tail1]) :- !,
  next_rule(Tail, Tail1).
next_rule([neg next A | Tail], [neg A| Tail1]) :- !,
  next_rule(Tail, Tail1).
next_rule([_ | Tail], Tail1) :-
  next_rule(Tail, Tail1).

%  Increment node counter.

get_num(Num) :- retract(num(Num)), Num1 is Num+1, assert(num(Num1)).

%  search(Path, FmlList, N) -
%    Search up Path for ancestor with same FmlList, return N.

search([], _, _) :- !, fail.
search([pt(Fmls1,N)|_], Fmls2, N) :- 
  subset(Fmls1, Fmls2), subset(Fmls2, Fmls1), % Set equality
  !.
search([_|Tail], Fmls, N) :-
  search(Tail, Fmls, N).


%  check_tableau(Tab, Result)
%    The result of the tableau is obtained by traversing the tableau.
%  tableau_result(Result1, Result2, Result)
%    Integrates the result
%      Two closed children imply closed,
%      one open is open is open,
%      empty is ignored,
%      otherwise connected, meaning that fulfilment must be checked.

check_tableau(empty,      empty)   :- !.
check_tableau(open,       open)    :- !.
check_tableau(closed,     closed)  :- !.
check_tableau(connect(_), connect) :- !.
check_tableau(t(_, Left, Right, _, _), Result) :-
  check_tableau(Left,  Result1),
  check_tableau(Right, Result2),
  tableau_result(Result1, Result2, Result).

tableau_result(empty, R, R) :- !.
tableau_result(R, empty, R) :- !.
tableau_result(open, _, open) :- !.
tableau_result(_, open, open) :- !.
tableau_result(closed, closed, closed) :- !.
tableau_result(_, _, connect).

%  create_states(Tab, Accumulator, States, Tau)
%    Create the States and transitions Tau from the tableau.
%    Accumulator accumulates formulas along a state path.
%      (1) For empty/open/closed, terminate recursion.
%      (2) For a state, the formulas are the union of those
%          in this node with those that have appeared on the path
%          since the last state.
%          Recurse on the subtrees with a clean accumulation of formulas.
%          Compute all transitions to this state.
%      (3) If not a state, recurse, accumulating formulas.

create_states(Tab, _, [], []) :-
  Tab \= t(_,_,_,_,_), !.

create_states(t(Fmls, Left, Right, N, Path), Union,
              [st(Union1, N) | States], Tau7) :-
  is_state(Fmls), !,
  union(Fmls, Union, Union1),
  create_states(Left, [], States1, Tau1),
  create_states(Right, [], States2, Tau2),
  append(States1, States2, States),
  append(Tau1, Tau2, Tau3),            % Transitions from subtrees.
  path1(Path, N, Tau4),                % Transition  from ancestor to N.
  append(Tau3, Tau4, Tau5),
  path2(Path, Fmls, Left, N, Tau6),    % Transition  from connected to.
  append(Tau5, Tau6, Tau7).

create_states(t(Fmls, Left, Right, _, _), Union, States, Tau) :-
  union(Fmls, Union, Union1),
  create_states(Left, Union1, States1, Tau1),
  create_states(Right, Union1, States2, Tau2),
  append(States1, States2, States),
  append(Tau1, Tau2, Tau).

%  is_state(Fmls) -
%    A set of Fmls is a state if it is not closed and
%      no alpha or beta rule applies.

is_state([])       :- !.
is_state([A | _])  :- alpha(A, _, _),     !, fail.
is_state([A | _])  :- beta(A,  _, _),     !, fail.
is_state(Fmls)     :- check_closed(Fmls), !, fail.
is_state([_|Tail]) :- is_state(Tail).

%  path(Path, N, Tau)
%    Search up path to first ancestor which is a state.

path1([], _, []).
path1([pt(Fmls,From) | _], To, [tau(From,To)]) :- 
  is_state(Fmls), !.
path1([_ | Tail], To, Tau) :-
  path1(Tail, To, Tau).

%  path2(Path, Fmls, Left, N, Tau)
%    If this state N is connected to a previous node To
%        (this is indicated in the Left subtree),
%      find a path back to To,
%      and reverse it to get the transition _to_ N.

path2(Path, Fmls, connect(To), N, Tau) :-
  path_from_connect([pt(Fmls,N) | Path], To, Path1),
  reverse(Path1, Path2),
  path1(Path2, N, Tau), !.
path2(_, _, _, _, []).

% path_from_connect(Path, To, NewPath)
%   Given Path, search up to create the subpath to
%     the child of To.

path_from_connect([], _, []) :- !.
path_from_connect([pt(_, To) | _], To, []) :- !.
path_from_connect([pt(Fmls, N) | Path], To, [pt(Fmls,N)|Path1]) :-
  path_from_connect(Path, To, Path1).

%  check_fulfilment(Tau, SCCs, Edges, States, Result)
%    Create the component graph from Tau and then check fulfilment.

check_fulfilment(_, [], _, _, _, [none]).
check_fulfilment(Tau, SCCs, Edges, States, Result) :-
  component_graph(Tau, SCCs, Edges),
  fulfil(SCCs, Edges, States, Result).

%  component_graph(Tau, SCCs, Edges)
%    Create the component graph from the transitions Tau.
%    Get the SCCs and then the set of edges between them.

component_graph(Tau, SCCs, Edges) :-
  retractall(tau(_,_)), assert_all(Tau),
  scc_list(SCCs),
  (setof(E, c_edge(E, SCCs), Edges) -> true; Edges=[]).

%  assert_all(Tau) - Assert all transitions into database.

assert_all([H|T]) :- assert(H), assert_all(T).
assert_all([]).

%  scc_list(SCCs)
%    Return a list of the SCCs formed by the database tau.

scc_list(SCCs) :-
  nodes(Nodes),          % Get all nodes
  scc_list1(Nodes, S),   % Get SCCs for each node
  list_to_set(S, SCCs).  % Remove duplicates

%  scc_list1(Nodes, S)
%    S is a list of SCCs on Nodes.
%    For each node N, get the SCC in which it occurs.

scc_list1([N|NTail], [S|STail]) :-
  scc(N,S), S \== [], !,
  scc_list1(NTail, STail).
scc_list1([_|NTail], STail) :-
  scc_list1(NTail, STail).
scc_list1([],[]).

%  nodes(Set) - Return the set of nodes occurring in the database tau.

nodes(Set) :-
  findall(N, tau(N,_), List),
  list_to_set(List, Set).


%  scc(Node, SCC)
%    Return the SCC of which Node is a member.
%    These are the set of nodes such that
%      each node is connected to N and conversely.

scc(Node, SCC) :-
  setof(N, (connected(Node,N), connected(N,Node)), SCC).

%  connected(From, To)
%  connected(From, To, Path)
%    From is connected to To by a Path on the transitions tau.

connected(From, From).
connected(From, To) :-
  connected(From, To, []).

connected(From, To, _) :-
  tau(From, To).
connected(From, To, Path) :- 
  tau(From, N), 
  \+ member(N, Path),
  connected(N, To, [N|Path]).

%  c_edge(E, SCCs)
%    E is an edge between SCCs,
%      if there are two different SCCs From, To,
%      nodes N1 in From, N2 in To, and
%      there is a transition from N1 to N2.

c_edge(e(From,To), SCCs) :-
  member(From, SCCs), member(To, SCCs), From \= To,
  member(N1, From),   member(N2, To),
  tau(N1, N2).

%  fulfil(SCCs, Edges, States, Result)
%  fulfil1(SCCs, Edges, States, Result)
%    Check fulfilment of SCCs.
%    Find a terminal SCC.
%    (1) If self-fulfilled, OK.
%    (2) Else delete SCC and recurse.

fulfil([], _, _, [none]).
fulfil(SCCs, Edges, States, Result) :-
  fulfil1(SCCs, Edges, States, Result).

fulfil1(SCCs, Edges, States, [ok(S)]) :-
  member(S, SCCs),                    % Some SCC
  \+ member(e(S, _), Edges),          % But no outgoing edge
  self_fulfil(S, States, Result),     % If self_fulfil returns ok...
  Result == ok, !.                    % ... result is ok.

fulfil1(SCCs, Edges, States, [notok(S, Result)|Result1]) :-
  member(S, SCCs),
  \+ member(e(S, _), Edges),
  self_fulfil(S, States, Result),     % Otherwise, result is not ok,
  delete(SCCs, S, SCCs1),             % so delete SCC and 
  delete(Edges, e(_,S), Edges1),      % edges into it and recurse.
  fulfil1(SCCs1, Edges1, States, Result1).

fulfil1(_, _, _, []).

%  self_fulfil(SCC, States, Result)
%  self_fulfil(SCC, OriginalSCC, States, Result)
%    For each state in each SCC,
%      check all future formulas are fulfilled
%      and integrate the results.

self_fulfil(SCC, States, Result) :-
  self_fulfil(SCC, SCC, States, Result).

self_fulfil([], _, _, ok).
self_fulfil([S|Tail], SCC, States, Result) :-
  member(st(Fmls,S), States),
  fulfil_all_future_formulas(Fmls, SCC, States, R),
  self_fulfil(Tail, SCC, States, Rest),
  fulfil_result(R, Rest, Result).

%  fulfil_all_future_formulas(Fmls, SCC, States, Result)
%    For each future formula in the state,
%      check if it is fulfilled.
%    If not, return it as the result so we can display
%      which formula was not fulfilled.
  
fulfil_all_future_formulas([], _, _, ok).
fulfil_all_future_formulas([F|Tail], SCC, States, R) :-
  future(F, F1),
  fulfil_future_formula(SCC, F1, States), !,
  fulfil_all_future_formulas(Tail, SCC, States, R).
fulfil_all_future_formulas([F|_], _, _, F) :-
  future(F, _), !.
fulfil_all_future_formulas([_|Tail], SCC, States, R) :-
  fulfil_all_future_formulas(Tail, SCC, States, R).

%  fulfil_result(R1, R2, R)
%    Integrate results: ok's propagate up,
%      but are stopped by failures to fulfil.

fulfil_result(ok, ok, ok) :- !.
fulfil_result(X,  ok, X)  :- !.
fulfil_result(_,   Y, Y).

%  future(F, F1)
%    F is a future formula which is fulfilled by F1.

future(eventually F, F).
future(neg always F, neg F).

%  fulfil_future_formula(SCC, F, States)
%    For each state N in the SCC,
%      get the formulas from the data structure States,
%      and check if F is in it.

fulfil_future_formula([N|_], F, States) :-
  member(st(Fmls,N), States),
  member(F, Fmls), !.
fulfil_future_formula([_|Tail], F, States) :-
  fulfil_future_formula(Tail, F, States).
