%
%
%                        LearnSat
%
%    Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.


%  Davis-Putnam-Logemann-Loveland (DPLL) algorithm with
%    conflict-driven clause learning (CDCL) and
%    non-chronological backtracking (NCB)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Export main predicate dpll/2 and the negation operator
:- module(dpll, [op(610, fy,  ~), dpll/2]).

%  Modules directly used by dpll
:- use_module([cdcl,counters,modes,display,dot,auxpred,io]).

%  Make housekeeping predicates visible after consulting dpll
:- reexport(modes, 
  [show_config/0, usage/0, set_display/1, clear_display/1,
   set_mode/1, set_learn_mode/1]).

:- reexport(auxpred, [set_order/1, get_order/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Data structures
%
%    Assignments:
%      assign(Variable, Value, Level, DecisionOrAntecedent)
%        DecisionOrAntecedent is 'yes' for decision assignments
%        and contains the antecedent assignment for implied assignments
%
%    Implication graph:
%      graph(Nodes, Edges)
%        A node is an assignment and an edge is a triple
%          edge(source assignment, clause number, target assignment)
%
%    Learned clauses are stored as a dynamic list
%      learned(list of clauses) in cdcl.pro
%
%    Non-chronological backtracking level is stored as a dynamic integer
%      backtrack(Level) in cdcl.pro
%
%    List of variables in the clause is stored as a dynamic list
%      variables_list(list of variables) in auxpred.pro
%      to enable the user to change the order of the variables


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Main predicate dpll/2
%    Performs initialization and calls auxiliary predicate dpll/6,
%      which either finds a unit (find_unit) and propagates it
%      or makes a decision assignment (choose_assignment) and evaluates
%    The predicate ok_or_conflict is called with the result and
%      decides whether to recurse or fail

%  dpll/2
%      Clauses   - set of clauses to be checked for satisfiability
%      Decisions - return decisions if the clauses are satisfiable
%                  or [] if the clauses are unsatisfiable

dpll(Clauses, Decisions) :-
      % Initialization
  init_modes,
  init_display,
  init_counters,
  init_tree,
  retractall(learned(_)),
  assert(learned([])),
  retractall(backtrack(_)),
  assert(backtrack(1)),

      % Create a set of variables from the list of clauses
  get_variables_of_clauses(Clauses, Variables_Set),
  display(clause, Clauses),

      % Call dpll/6, initially with Level 0, no assignments, empty graph
  dpll(Clauses, Variables_Set, 0, [], graph([],[]), Decisions),
  display(result, Decisions).

%  dpll/6 failed so return unsatisfiable: empty list of assignments
dpll(_, []) :-
  display(result, []).


%  dpll/6
%      Clauses    - set of clauses
%      Variables  - list of unassigned variables
%      Level      - deepest decision level of assignments
%      SoFar      - assignments so far
%      Graph      - implication graph
%      Decisions  - return decisions

%  No more variables need to be assigned so the set is satisfiable
dpll(_, [], _, Decisions, _, Decisions) :- !,
  display(tree, Decisions, sat).

%  Attempt unit propagation
dpll(Clauses, Variables, Level, SoFar, Graph, Decisions) :-
      % Check for unit clauses, including the learned clauses
      % find_unit returns the clause that became a Unit and
      %   the Assignment forced by the unit clause
  add_learned_clauses(Clauses, Clauses1),
  find_unit(Clauses1, Level, SoFar, Unit, Assignment), !,
      % Increment the unit counter, convert the Assignment to a Literal
  New_Assignment = [Assignment | SoFar],
  increment(unit),
  display(unit, Assignment, Unit, Clauses1),
  display(caused, SoFar),
  display(partial, New_Assignment),
      % Evaluate the set of Clauses using the assignments SoFar
      %   together with the Assignment returned by find_unit
      % Return the Result (ok or conflict),
      %   and the Conflict clause if the result is a conflict
  evaluate(Clauses1, New_Assignment, Conflict, Result),
      % Get the new implication Graph1, adding the new Assignment
      %   that was forced by the unit clause
      % The antecedent or the Number of the antecedent
      %   that became a Unit labels the new edges
  nth1(Number, Clauses1, Unit),
  extend_graph(Unit, Number, Assignment, SoFar, Graph, Graph1),
  display(incremental, Graph1, Clauses),
  display(dot_inc, Graph1, Clauses),
      % Call ok_or_conflict with the Result of the evaluation,
      %   the Conflict clause, the new Graph1 and add the new Assignment
  ok_or_conflict(
    Result, Variables, Clauses1,
    New_Assignment, Level, Graph1, Conflict, Decisions).

%  Choose a decision assignment
dpll(Clauses, Variables, Level, SoFar, Graph, Decisions) :-
      % Increment the assignment Level and set the backtrack level to it
  Level1 is Level + 1,
  retract(backtrack(_)),
  assert(backtrack(Level1)),
      % Choose a decision Assignment and increment the decision counter
  choose_assignment(Variables, Level1, Assignment),
  New_Assignment = [Assignment | SoFar],
  increment(decision),
  display(variable, Variables),
  display(decision, Assignment),
  display(partial, New_Assignment),
      % Evaluate the set of Clauses including the learned clauses
      %   using the assignments SoFar and the chosen Assignment
      % Return the Result (ok or conflict),
      %   and the Conflict clause if the result is a conflict
  add_learned_clauses(Clauses, Clauses1),
  evaluate(Clauses1, New_Assignment, Conflict, Result),
      % Call ok_or_conflict with the Result of the evaluation,
      %   the Conflict clause, the new Graph1 and add the new Assignment
  ok_or_conflict(
    Result, Variables, Clauses1,
    New_Assignment, Level1, Graph, Conflict, Decisions).


%  ok_or_conflict/7
%    Check the result of the evaluation
%      Status    - ok or conflict
%      Variables - list of unassigned variables
%      Clauses   - set of clauses
%      SoFar     - assignments so far
%      Level     - deepest decision level of assignments so far
%      Graph     - implication graph
%      Conflict  - conflict clause in case of conflict
%      Decisions - return decisions

%  Conflict: learn a clause and fail
ok_or_conflict(conflict, _, Clauses, SoFar, Level, Graph, Conflict, _) :-
      % Increment the conflict counter
  increment(conflict),
  display(conflict, Conflict, Clauses),
  display(assignment, SoFar),
  display(tree, SoFar, conflict),
      % Add the "kappa" node for the conflict clause to the graph
  nth1(Number, Clauses, Conflict),
  extend_graph(Conflict, Number, kappa, SoFar, Graph, Graph1),
      % Display the implication graph
  display(graph, Graph1, Clauses),
      % Compute the learned clause and save in the database
  compute_learned_clause_by_dominator(Graph1, Level, Dominator),
  compute_learned_clause_by_resolution(Graph1, Conflict, SoFar, Level),
      % Write the implication graph to the dot file
      %   using the Level and Dominator for emphasis
  display(dot, Graph1, Clauses, Level, Dominator),
      % Fail on conflict
  fail.

%  OK: delete the assigned variable from the unassigned Variables
%      and recurse on dpll/6
ok_or_conflict(ok, Variables, Clauses, SoFar, Level, Graph, _, Decisions) :-
  display(tree_inc, SoFar, ok),
  SoFar = [assign(V, _, _, _) | _], 
  delete_variable(Variables, V, Variables1),
  dpll(Clauses, Variables1, Level, SoFar, Graph, Decisions).


%  add_learned_clauses(Clauses, Clauses1)
%    Add the learned clauses to the set of clauses
%      Clauses  - the given set of clauses
%      Clauses1 - the set together with the learned clauses
add_learned_clauses(Clauses, Clauses1) :-
  learned(Learned),
  union(Clauses, Learned, Clauses1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Finding a unit and evaluating a clause
%
%  find_unit/5
%    Find a unit clause
%      Clauses    - set of clauses
%      Level      - decision level is needed to create an assignment
%      SoFar      - assignments made so far
%      Unit       - return a clause that becomes a unit
%      Assignment - return the assignment forced by the unit

%  Fail since no unit has been found by the end of the list of clauses
find_unit([], _, _, _, _) :- !, fail.

%  Evaluating the head of the list returns a unit
%  Create an assignment from the literal
find_unit([Head | _], Level, SoFar, Head, Assignment) :-
    %  [] for second argument prevents duplicate display of the clause
  evaluate_clause(Head, [], SoFar, notfound, Unit, Result),
  Result = unit, !,
    % Create non-decision assignment; Head is the antecedent clause
  to_assignment(Unit, Level, Head, Assignment).

%  The clause was not a unit so recurse on the rest of the clauses
find_unit([_ | Tail], Level, SoFar, Unit, Assignment) :-
  find_unit(Tail, Level, SoFar, Unit, Assignment).


%  evaluate/4
%    Evaluate a set of clauses   
%      Clauses     - the set of clauses
%      Assignments - the assignments used to evaluate the clauses
%      Conflict    - return a conflict clause
%      Result      - return a result ok or conflict

%  No conflict found by the end of the list of clauses, so return ok
evaluate([], _, _, ok).

%  Evaluating the first clause returns unsatisfied, so return conflict
evaluate([Head | _], Assignments, Head, conflict) :-
  evaluate_clause(Head, Head, Assignments, notfound, _, Result),
  Result = unsatisfied, !.

%  The clause not unsatisfied, so recurse on the rest of the clauses
evaluate([_ | Tail], Assignments, Conflict, Result) :-
  evaluate(Tail, Assignments, Conflict, Result).


%  evaluate_clause/6
%    Evaluate a single clause
%      Clause      - the clause
%      Original    - original clause for displaying
%      Assignments - the assignments used to evaluate the clause
%      Found       - a flag that an unassigned literal has been "found"
%                  - set to "notfound" in the initial call
%                  - if another unassigned literal is found when the
%                      flag is "found", the clause is not a unit
%      Unit        - if the result is unit, return the literal that
%                      forms the unit clause
%      Result      - return satisfied, unsatisfied, unit, unresolved


%  End of the list of literals in the clause:
%    If a single unassigned literal is "found", this is a unit clause
evaluate_clause([], Original, _, found, _, unit) :- !,
  display(evaluate, Original, ' is a unit clause', none).

%    Otherwise, the clause is unsatisfied
evaluate_clause([], Original, _, _, _, unsatisfied) :-
  display(evaluate, Original, ' is false', none).

%  If a literal is assigned 1 (true), return satisfied
evaluate_clause([Head | _], Original, Assignments, _, _, satisfied) :-
  is_assigned(Head, Assignments, 1), !,
  display(evaluate, Original, ' is true', none).

%  If a literal is assigned 0 (false), recurse on the remaining literals
evaluate_clause([Head | Tail], Original, Assignments, Found, Unit, Result) :-
  is_assigned(Head, Assignments, 0), !,
  display(evaluate, Original, ' has literal ', Head),
  evaluate_clause(Tail, Original, Assignments, Found, Unit, Result).

%  If an unassigned literal is found:
%    if one has been found before, the clause is unresolved
evaluate_clause([_| _], _, _, found, _, unresolved) :- !.

%    otherwise, continue searching with found flag set
%    and unify the Head as the Unit returned
evaluate_clause([Head | Tail], Original, Assignment, _, Head, Result) :-
  evaluate_clause(Tail, Original, Assignment, found, _, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Choose a decision assignment
%  When backtracking, another one will be chosen

%  choose_assignment/3
%      Variables  - list of unassigned variables
%                   the first unassigned variable is assigned
%      Level      - record the decision level in the assignment
%      Assignment - return the decision assignment
%
%  choose_value/2
%      Variable - variable to assign to
%      N - value returned 0 or 1
%    If Variable is a negated literal, choose 1 first and then 0

choose_assignment([V | _], Level, Assignment) :-
      % Build the Assignment term as a Decision assignment (yes)
  to_variable(V, V1),
  Assignment = assign(V1, N, Level, yes),
      % Choose a value for the assignment
  choose_parity(V, N),
      % Non-chronological backtracking when mode is "ncb"
      %   if the current Level is greater than the backtrack level, fail
      % Cut within "if->then;else" is local and does _not_ destroy the
      %   choice points for "member" and ";" above
  alg_mode(Mode),
  backtrack(L),
  (Mode = ncb, Level > L, Level > 1 ->
    display(skipped, Assignment), !, fail ;
    true).

choose_parity(~_, N) :- !,
  (N = 1 ; N = 0).
choose_parity(_, N) :-
  (N = 0 ; N = 1).
