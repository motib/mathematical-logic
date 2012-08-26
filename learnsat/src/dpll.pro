% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%    LearnSat

%  Davis-Putnam-Logemann-Loveland (DPLL) algorithm with
%    conflict-driven clause learning (CDCL) and
%    non-chronological backtracking NCB

:- module(dpll, [op(610, fy,  ~), dpll/2]).

:- use_module([config,counters,display]).

:- reexport(display, 
  [show_config/0, usage/0, set_display/1, clear_display/1, set_mode/1]).


%  Data structures
%    Assignments: assign(Variable, Value, Level, IsDecisionAssignment)
%    Implication graph: graph(Nodes, Edges)
%    Learned clauses (dynamic): learned(Clauses)
%    Non-chronological backtracking level (dynamic): backtrack(Level) 

:- dynamic learned/1.
:- dynamic backtrack/1.

%  dpll/2
%    Exported predicate for performing the DPLL algorithm
%      Clauses    - set of clauses
%      Decisions  - return decisions for satisfiable or []

dpll(Clauses, Decisions) :-
      % Display copyright notice
  version(V),
  write('LearnSAT v'),
  write(V),
  write('. Copyright 2012 by Moti Ben-Ari. GNU GPL.\n'),
      % Initialize the display options and display the clauses
  display(clauses, Clauses),
  init_display,
      % Create a set of variables from the list of clauses
  flatten(Clauses, Literals_List),
  literals_to_variables(Literals_List, [], Variables_Set),
      % Initialize the counters, the mode and the dynamic databases
  init_counters(Clauses, Variables_Set),
  init_mode,
  retractall(learned(_)),
  assert(learned([])),
  retractall(backtrack(_)),
  assert(backtrack(1)),
      % Call the auxiliary predicate dpll1
      % Initially: Level 0, no assignments, empty graph
  dpll1(Clauses, Variables_Set, 0, [], graph([],[]), Decisions).

%  If dpll1 fails, the clauses are unsatisfiable
dpll(_, []) :-
  display(result, unsatisfiable, []).

           
%  dpll1/6
%    Predicate for performing the DPLL algorithm
%      Clauses    - set of clauses
%      Variables  - list of unassigned variables
%      Level      - deepest decision level of assignments
%      SoFar      - assignments so far
%      Graph      - implication graph
%      Decisions  - return decisions

%  No more variables need to be assigned so the set is satisfiable
dpll1(_, [], _, Decisions, _, Decisions) :- !,
  display(result, satisfiable, Decisions).

%  Unit propagation
dpll1(Clauses, Variables, Level, SoFar, Graph, Decisions) :-
      % Check for unit clauses
      % Using also the learned clauses
      % The Unit clause and its Assignment are returned 
  add_learned_clauses(Clauses, Clauses1),
  find_unit(Clauses1, Level, SoFar, Unit, Assignment), !,
      % Increment the unit counter and display the unit clause 
  increment(unit),
  to_literal(Assignment, Literal),
  display(unit, Literal, Unit, Clauses1),
  display(caused, SoFar),
      % Evaluate the set of Clauses using the assignments SoFar
      %   together with the Assignment returned by find_unit
  evaluate(Clauses1, [Assignment | SoFar], Conflict, Result),
      % Get the new implication graph, adding the new assignment
      % The Number of the unit clause will label the new edges
  nth1(Number, Clauses1, Unit),
  get_graph(Unit, Number, Assignment, SoFar, Graph, Graph1),
  display(incremental, Graph1),
      % Call ok_or_conflict with the result of the evaluation
  ok_or_conflict(
    Result, Variables, Level, Clauses1,
    [Assignment | SoFar], Graph1, Conflict, Decisions).

%  Choose a decision assignment
dpll1(Clauses, Variables, Level, SoFar, Graph, Decisions) :-
      % Increment the assignment level and set the backtrack level
  Level1 is Level + 1,
  retract(backtrack(_)),
  assert(backtrack(Level1)),
      % Choose a decision assignment
  choose_assignment(Variables, Level1, Assignment),
      % Increment the choice counter and display the decision
  increment(choice),
  display(variables, Variables),
  display(decision, Assignment),
      % Evaluate the set of Clauses using the assignments SoFar
      %   together with the Assignment that was chosen
  add_learned_clauses(Clauses, Clauses1),
  evaluate(Clauses1, [Assignment | SoFar], Conflict, Result),
      % Call ok_or_conflict with the result of the evaluation
  ok_or_conflict(
    Result, Variables, Level1, Clauses1,
    [Assignment | SoFar], Graph, Conflict, Decisions).


%  ok_or_conflict/7
%    Check the result of the evaluation
%      Status    - ok or conflict
%      Variables - list of unassigned variables
%      Level     - deepest decision level of assignments so far
%      Clauses   - set of clauses
%      SoFar     - assignments so far
%      Graph     - implication graph
%      Conflict  - conflict clause in case of conflict
%      Decisions - return decisions

%  Conflict: learn a clause and fail
ok_or_conflict(conflict, _, Level, Clauses, SoFar, Graph, Conflict, _) :-
      % Increment the conflict counter and display the conflict clause
  increment(conflict),
  display(conflict, Conflict, Clauses),
  display(assignments, SoFar),
      % Add the "kappa" node for the conflict clause to the
      %   implication graph
  nth1(Number, Clauses, Conflict),
  get_graph(Conflict, Number, kappa, SoFar, Graph, Graph1),
      % Display the graph and write the dot file
  display(graph, Graph1),
  display(dot, Graph1),
      % Compute the learned clause and save in the database
      %   (but not if in dpll mode)
  get_mode(Mode),
  Mode \= dpll,
  compute_learned_clause(Graph1, Clauses, Level),
      % Fail on conflict
  fail.

%  OK: delete the assigned variable and recurse
ok_or_conflict(ok, Variables, Level, Clauses, SoFar, Graph, _, Decisions) :-
  SoFar = [assign(V, _, _, _) | _], 
  delete(Variables, V, Variables1),
  dpll1(Clauses, Variables1, Level, SoFar, Graph, Decisions).


%  find_unit/5
%    Find a unit clause
%      Clauses    - set of clauses
%      Level      - decision level is needed to create an assignment
%      SoFar      - assignments made so far
%      Unit       - return a unit clause
%      Assignment - return the assignment implied by the unit

%  Fail since no unit has been found by the end of the list of clauses
find_unit([], _, _, _, _) :- !, fail.

%  Evaluating the head of the list returns a unit
find_unit([Head | _], Level, SoFar, Head, Assignment) :-
  evaluate_clause(Head, SoFar, notfound, Unit, Result),
  Result = unit, !,
      % Create an assignment from the literal
      %   The assignment is not a decision assignment
  to_assignment(Unit, Level, no, Assignment).

%  A clause was not a unit so recurse on the rest of the clauses
find_unit([_ | Tail], Level, SoFar, Unit, Assignment) :-
  find_unit(Tail, Level, SoFar, Unit, Assignment).


%  evaluate/4
%    Evaluate a set of clauses   
%      Clauses     - the set of clauses
%      Assignments - the assignments with which to evaluate the clauses
%      Conflict    - return a conflict clause
%      Result      - return a result ok or conflict

%  No conflict found by the end of the list of clauses, so return ok
evaluate([], _, _, ok).

%  Evaluating the first clause returns unsatisfied, so return conflict
evaluate([Head | _], Assignments, Head, conflict) :-
  evaluate_clause(Head, Assignments, notfound, _, Result),
  Result = unsatisfied, !.

%  A clause not unsatisfied, so recurse on the rest of the clauses
evaluate([_ | Tail], Assignments, Conflict, Result) :-
  evaluate(Tail, Assignments, Conflict, Result).


%  evaluate_clause/5
%    Evaluate a single clause
%      Clause      - the clause
%      Assignments - the assignments with which to evaluate the clause
%      Found       - a flag that an unassigned literal has been found
%                  - set to notfound in the initial call evaluate_clause
%      Unit        - if the result is unit, return the unit
%      Result      - return satisfied, unsatisfied, unit, unresolved

%  End of the list of literals in the clause:
%    if a single unassigned literal is found, this is a unit clause
evaluate_clause([], _, found, _, unit) :- !.

%  End of the list of literals in the clause
%    otherwise, the clause is unsatisfied
evaluate_clause([], _, _, _, unsatisfied).

%  If a literal is assigned 1 (true), return satisfied
evaluate_clause([Head | _], Assignments, _, _, satisfied) :-
  is_assigned(Head, Assignments, 1), !.

%  If a literal is assigned 0 (false), recurse on the remaining literals
evaluate_clause([Head | Tail], Assignments, Found, Unit, Result) :-
  is_assigned(Head, Assignments, 0), !,
  evaluate_clause(Tail, Assignments, Found, Unit, Result).

%  If an unassigned literal is found:
%    if one has been found before, the clause is unresolved
evaluate_clause([_| _], _, found, _, unresolved) :- !.

%  If an unassigned literal is found:
%    otherwise, continue searching with found flag set
%  The literal is returned as the unit
evaluate_clause([Head | Tail], Assignment, notfound, Head, Result) :-
  evaluate_clause(Tail, Assignment, found, _, Result).


%  choose_assignment/3
%    Choose an assignment
%    When backtracking, another one will be chosen
%      Variables  - list of unassigned variables
%      Level      - record the decision level in the assignment
%      Assignment - return the decision assignment

choose_assignment(Variables, Level, Assignment) :-
      % Build the Assignment
  Assignment = assign(V, N, Level, yes),
      % Choose a variable from the list of unassigned variables
  member(V, Variables),
      % Choose a value (0 or 1)
  choose_value(N),
      % Non-chronological backtracking (only in ncb mode)
      %   if the current Level is greater than the backtrack level, fail
  get_mode(Mode),
  backtrack(L),
      % Fail within Condition -> Action assures that
      %   the alternative true will not be attempted
  (Level > L, Level > 1, Mode = ncb ->
    display(skipping, Assignment), !, fail ; true).


%  choose_value/1
%    Value      - return a value

choose_value(0).
choose_value(1).


%  get_graph/6
%    Extend the implication graph for a unit clause
%      Clause     - unit clause
%      Number     - number of the unit clause
%      Assignment - current assignment
%      SoFar      - assignments so far
%      Graph      - current implication graph
%      Graph1     - return the new implication graph

%  If the Assignment is to this literal, don't add to graph
get_graph([Head | Tail], Number, Assignment, SoFar, Graph, Graph1) :-
  to_variable(Head, Variable),
  Assignment = assign(Variable, _, _, _), !, 
  get_graph(Tail, Number, Assignment, SoFar, Graph, Graph1).

%  Otherwise, search for an assignment to this literal
%    which will become a new source node with an edge to Assignment
get_graph([Head | Tail], Number, Assignment, SoFar, 
          graph(Nodes, Edges), Graph1) :-
  to_variable(Head, Head1),
  Source = assign(Head1, _, _, _),
  member(Source, SoFar),
      % If found, add this assignment as a source node
      %   and add the (Number of the) antecedent unit clause
      %   as a new edge
  get_graph(Tail, Number, Assignment, SoFar,
    graph(
      [node(Source) | Nodes],
      [edge(Source, Number, Assignment) | Edges]), Graph1).

%  End of the clause, add Assignment as a target node
%  Sort the nodes and edges (this removes duplicates, if any)
get_graph([], _, Assignment, _, 
          graph(Nodes, Edges), graph(Nodes1, Edges1)) :-
  sort([node(Assignment) | Nodes], Nodes1),
  sort(Edges, Edges1).


%  compute_learned_clause/3
%    Compute a learned clause from an implication graph
%      Graph   - an implication graph
%      Clauses - the set of clauses
%      Level   - the current (highest) level

compute_learned_clause(Graph, Clauses, Level) :-
  Graph = graph(Nodes, Edges),
      % Search for the antecedent of the kappa node
  member(edge(_, N, kappa), Edges), !,
  nth1(N, Clauses, Clause),
      % Compute the Learned clauses backwards from this clause
  compute_learned_clause1(Graph, Clauses, Level, Clause, Learned),
  display(learned, Learned),
      % Compute the backtrack Level from the Learned clause
  compute_backtrack_level(Learned, Level, 0, Nodes),
      % Add the Learned clause to the list of learned clauses
  retract(learned(List)),
  union([Learned], List, List1),
  assert(learned(List1)).


%  compute_learned_clause1/5
%    Construct the learned clause from the antecedents
%      Graph   - the implication graph
%      Clauses - the set of clauses
%      Level   - the current (highest) level
%      Clause  - the current clause being resolved
%      Learned - return the learned clause

%  Stop the search if a unique implication point (uip) has been reached
compute_learned_clause1(graph(Nodes, _), _, Level, Clause, Clause) :-
  check_uip(Clause, Nodes, Level, 0), !.

%  Search for a literal that can be resolved
compute_learned_clause1(Graph, Clauses, Level, Clause, Learned) :-
  Graph = graph(_, Edges),
      % Find a literal in the Clause
  member(Literal, Clause),
      %   and a clause that contains its complement which is
      %   assigned at the same Level and is not a decision node
  to_complement(Literal, Literal1),
  to_assignment(Literal1, Level, no, Assignment),
  member(edge(_, N, Assignment), Edges), !,
  nth1(N, Clauses, Clause1),
      % Resolve and recurse
  resolve(Literal, Clause, Clause1, Clause2),
  display(resolvent, Clause, Clause1, Clause2),
  compute_learned_clause1(Graph, Clauses, Level, Clause2, Learned).

% If no more such clause pairs exist, return the learned clause
compute_learned_clause1(_, _, _, Clause, Clause).


%  check_uip/4
%    Check if a clause is a unique implication point
%      Clause  - the current clause
%      Nodes   - the nodes of the implication graph
%      Level   - the current (highest) level
%      Number  - the number of literals assigned at this level

%  If two literals are assigned at this level, the clause is not a uip
check_uip(_, _, _, 2) :- !,
  display(uip, no),
  fail.

%  If one literal is assigned at this level, the clause is a uip
check_uip([], _, _, 1) :-
  display(uip, yes).

% For a literal, check if its variable is assigned at this level
check_uip([Head | Tail], Nodes, Level, N) :-
  to_variable(Head, Variable),
  member(node(assign(Variable, _, Level, _)), Nodes), !,
    % If so, increment the number assigned and recurse
  display(literal, Head),
  N1 is N+1,
  check_uip(Tail, Nodes, Level, N1).

% The variable of the literal was not assigned at this level, recurse
check_uip([_ | Tail], Nodes, Level, N) :-
  check_uip(Tail, Nodes, Level, N).


%  resolve/4
%    Resolve two clauses on a given literal
%      Literal   - literal from Clause1 to resolve on
%      Clause1   - first clause
%      Clause2   - second clause with complement of Literal
%      Resolvent - return resolvent clause
resolve(Literal, Clause1, Clause2, Resolvent) :-
  delete(Clause1, Literal, Clause11),
  to_complement(Literal, Literal1),
  delete(Clause2, Literal1, Clause21),
  union(Clause11, Clause21, Resolvent).


%  add_learned_clauses(Clauses, Clauses1)
%    Add the learned clauses to the set of clauses
%      Clauses  - the given set of clauses
%      Clauses1 - the set together with the learned clauses
add_learned_clauses(Clauses, Clauses1) :-
  learned(Learned),
  union(Clauses, Learned, Clauses1).


%  compute_backtrack_level/4
%    Compute the non-chronological backtrack level as the highest
%      level of an assignment for the learned clause (except for
%      the current level)
%        Learned  - learned clause
%        Level    - current level
%        Highest  - highest level so far
%        Nodes    - nodes with assignments in the implication graph

%  At end of learned clause, save the highest level
compute_backtrack_level([], _, Highest, _) :-
  display(backtrack, Highest),
  retract(backtrack(_)),
  assert(backtrack(Highest)).

%  Check each literal of the learned clause
compute_backtrack_level([Head | Tail], Level, Highest, Nodes) :-
      % Search for an assignment to the complement of a literal
  to_complement(Head, Head1),
  to_assignment(Head1, L, _, Assignment),
  member(node(Assignment), Nodes),
      % Save if higher than seen so far (but not the current level)
  L =\= Level, L > Highest, !,
  compute_backtrack_level(Tail, Level, L, Nodes).

%  Otherwise, recurse
compute_backtrack_level([_ | Tail], Level, Highest, Nodes) :-
  compute_backtrack_level(Tail, Level, Highest, Nodes).
  

%  is_assigned/3
%    Check if a literal is assigned and fail if not
%      Literal      - check if this literal is assigned
%      Assignments  - the assignments to be checked
%      Value        - return the value of the literal

is_assigned(~ Variable, Assignments, Value1) :- !,
  member(assign(Variable, Value, _, _), Assignments),
      % The literal is a negated atom, so return the opposite value
  Value1 is 1-Value.
is_assigned(Variable, Assignments, Value) :-
  member(assign(Variable, Value, _, _), Assignments).


%  literals_to_variables/2
%    Get the set of variables of a list of literals
%      Literals      - a list of literals
%      SoFar         - the variables found so far
%      Variables_Set - the set of variables of these literals

literals_to_variables([], Variables_List, Variables_Set) :-
    % The predicate sort removes duplicates
  sort(Variables_List, Variables_Set).
literals_to_variables([V | Tail], SoFar, Variables_Set) :-
  to_variable(V, V1),
  literals_to_variables(Tail, [V1 | SoFar], Variables_Set).


%  to_variable/2
%    Get the variable of a literal
%      Literal  - a literal
%      Variable - the variable of that literal

to_variable(~ Variable, Variable) :- !.
to_variable(Variable,   Variable).


%  to_complement/2
%    Get the complement of a literal
%      Literal  - a literal
%      Literal1 - the complement of Literal

to_complement(~ Variable, Variable) :- !.
to_complement(Variable,   ~ Variable).


%  to_assignment/4
%    Literal    - a literal
%    Level      - a decision level
%    Decision   - is this a assignment or not
%    Assignment - the literal expressed as an assignment

to_assignment(~ Variable,
  Level, Decision, assign(Variable, 0, Level, Decision)) :- !.
to_assignment(Variable,
  Level, Decision, assign(Variable, 1, Level, Decision)).


%  to_literal/2
%    Assignment - an assignment
%    Literal    - the assignment as a literal
%  Example: assign(p1, 0, 3, no) becomes ~p1

to_literal(assign(V, 0, _, _), ~V).
to_literal(assign(V, 1, _, _), V).
