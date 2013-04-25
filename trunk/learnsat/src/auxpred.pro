% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- module(auxpred,
  [is_assigned/3, literals_to_variables/3, get_variables_of_clauses/2,
   to_variable/2, to_complement/2, to_assignment/4,
   to_literal/2, is_negative/2, to_complemented_clause/2,
   set_order/1, clear_order/0, get_order/1, delete_variable/3]).  

:- use_module([counters]).

:- op(610, fy,  ~).

:- dynamic variables_list/1.

%  Auxiliary predicates concerned with assignments, variables, literals


%     Order of assignment to variables
%  set_order/1
%    Variables are assigned in the order of their appearance in List
%    The variables can be literals: if negative assign 1 before 0
%  clear_order/0
%    Restore default: lexicographical order
%  get_order/1
%    Return order to display configuration

set_order(List) :-
  retractall(variables_list(_)),
  assert(variables_list(List)).

clear_order :-
  retractall(variables_list(_)).

get_order(List) :-
  variables_list(List), !.
get_order(default).


%  delete_variable/3
%      Variables - list of variables
%      V - variable to delete
%      Variables1 - list of variables after deletion of V
%    The list of "variables" includes literals if the order is specified
%      so check to delete both V and ~V 

delete_variable(Variables, V, Variables1) :-
  member(V, Variables), !,
  delete(Variables, V, Variables1).
delete_variable(Variables, V, Variables1) :-
  member(~V, Variables), !,
  delete(Variables, ~V, Variables1).


%  is_assigned/3
%    Check if a literal is assigned and if so return its value
%    Fail if not assigned
%      Literal      - check if this literal is assigned
%      Assignments  - the assignments to be checked
%      Value        - return the value of the literal

    % The literal is a negated assigned atom; return its complement
is_assigned(~ Variable, Assignments, Value1) :- !,
  member(assign(Variable, Value, _, _), Assignments),
  Value1 is 1-Value.
    % The literal is an assigned atom; return the value
is_assigned(Variable, Assignments, Value) :-
  member(assign(Variable, Value, _, _), Assignments).


%  get_variables_of_clauses/2
%    Get a sorted list of variables from a set of clauses
%    The predicate sort removes duplicates

get_variables_of_clauses(Clauses, Variables) :-
  flatten(Clauses, Literals_List),
  literals_to_variables(Literals_List, [], Variables_List),
  sort(Variables_List, Sorted_Variables_List),
  order_variables(Sorted_Variables_List, Variables),
  length(Clauses, Number_of_Clauses),
  length(Variables, Number_of_Variables),
  init_input_counters(Number_of_Clauses, Number_of_Variables).

order_variables(Variables_List, Variables) :-
  variables_list(Variables),
  literals_to_variables(Variables, [], Variables1),
  permutation(Variables_List, Variables1), !.
order_variables(_, Variables) :-
  variables_list(Variables), !,
  write('Ordered list of variables not a permutation of the variables in the clauses\n'),
  abort.
order_variables(Variables_List, Variables_List).


%  literals_to_variables/3
%    Get the set of variables of a list of literals
%      Literals      - a list of literals
%      SoFar         - the variables found so far
%      Variables_Set - the set of variables of these literals

literals_to_variables([], Variables_List, Variables_List).
literals_to_variables([V | Tail], SoFar, Variables_List) :-
  to_variable(V, V1),
  literals_to_variables(Tail, [V1 | SoFar], Variables_List).


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
%    Constructor for the term assign/4
%      Literal    - a literal
%      Level      - a decision level
%      Decision   - is this a decision assignment?
%                   yes or antecedent clause
%      Assignment - the literal expressed as an assignment

to_assignment(~ Variable, Level, Decision,
              assign(Variable, 0, Level, Decision)) :- !.
to_assignment(Variable,   Level, Decision,
              assign(Variable, 1, Level, Decision)).

%  is_negative/2
%    Is a Literal negative?
%    If so, return Variable; if not, fail
is_negative(~ Variable, Variable).


%  to_literal/2
%    Assignment - an assignment
%    Literal    - the assignment as a literal
%  Example: assign(p1, 0, 3, no) becomes ~p1

to_literal(assign(V, 0, _, _), ~V).
to_literal(assign(V, 1, _, _), V).

%  to_complemented_clause/2
%    List of assignments to a list of complemented literals
%      Assignments - a list of assignments
%      Clause      - the corresponding clause

to_complemented_clause([], []).
to_complemented_clause([Head1 | Tail1], [Head2 | Tail2]) :-
  to_complemented_literal(Head1, Head2),
  to_complemented_clause(Tail1, Tail2).

%  to_complemented_literal/2
%    Assignment - an assignment
%    Literal    - the assignment as a literal after complementing
%  Example: assign(p1, 1, 3, no) becomes ~p1

to_complemented_literal(assign(V, 0, _, _), V).
to_complemented_literal(assign(V, 1, _, _), ~V).

