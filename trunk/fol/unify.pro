% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Unification algorithm by Martelli/Montanari.

:- module(unify, [unify/3]).

%  unify(A, B, Substitution)
%    Returns an mgu in Substitution if A can be unified.
%    If not: returns [failure3,Fml], or [failure4,Fml]
%      if A and B cannot be unified due to failure in rule 3 or 4.

unify(A1, A2, Subst) :-
  A1 =.. [Pred | Args1],
  A2 =.. [Pred | Args2],
  create_equations(Args1, Args2, Eq),
  solve(Eq, Subst).

%  create_equations(Arg1, Arg2, Eq)
%    Creates a list of equations from argument lists Arg1, Arg2.

create_equations([Head1 | Tail1],
              [Head2 | Tail2], 
              [Head1 eq Head2 | List]) :- !,
  create_equations(Tail1, Tail2, List).
create_equations([], [], []).

%  solve(Eq, Substitution)
%    As in unify, but A and B are now a list of equations Eq.
%  solve(Front, Back, Status, Substitution)
%    The list of equations is maintained as two partial
%      lists Front, Back, where the Current equation is the
%      head of the list Back.
%    Status is used to stop the algorithm when:
%      the initial status notmodified has not been changed
%        after traversing the entire list, or,
%      failure3 or failure4 is set.

solve(Eq, Subst) :-
  solve([], Eq, notmodified, Subst).

% Stop algorithm upon failure, return equation which caused it.

solve(_, [Current|_], failure3, [failure3, Current]) :- !.
solve([Current], _,   failure4, [failure4, Current]) :- !.

% Try rules 1-4 on Current.
%   (1) Current is modified and put back on list.
%   (2) Current is deleted.
%   (3) Current is deleted and new equations appended to front of Back.
%   (4) Substitute Current in both Front and Back; let Current be Front.

solve(Front, [Current | Back], _, Result) :-
  rule1(Current, Current1), !,
  solve(Front, [Current1 | Back], modified, Result).
solve(Front, [Current | Back], _, Result) :-
  rule2(Current), !,
  solve(Front, Back, modified, Result).
solve(Front, [Current | Back], _, Result) :-
  rule3(Current, NewList, Status), !,
  append(NewList, Back, NewBack),
  solve(Front, NewBack, Status, Result).
solve(Front, [Current | Back], _, Result) :-
  append(Front, Back, List),
  rule4(Current, List, NewList, Status), !,
  solve([Current], NewList, Status, Result).
     
%  No rule applies:
%    continue with next equation,
%    if none, restart from beginning of list if modified,
%    otherwise, terminate.

solve(Front, [Current | Rest], Mod, Result) :- !,
  append(Front, [Current], NewFront),
  solve(NewFront, Rest, Mod, Result).
solve(List, [], modified, Result) :- !,
  solve([], List, notmodified, Result).
solve(Result, [], _, Result).

rule1(T eq X, X eq T) :- nonvar(T), var(X).   % Exchange sides of equation.

rule2(X eq Y)         :- X == Y.              % Same variable X eq X.

rule3(T1 eq T2, List, modified) :-            % Unpack f(...) eq f(...)
  nonvar(T1),                                 % Needed by ISO Prolog
  nonvar(T2),
  T1 =.. [F | Subterms1],
  T2 =.. [F | Subterms2],
  create_equations(Subterms1, Subterms2, List).

rule3(T1 eq T2, [T1 eq T2], failure3) :-      % Fail if functors different.
  nonvar(T1),                                 % Needed by ISO Prolog
  nonvar(T2),
  functor(T1, F1, _),
  functor(T2, F2, _),
  F1 \== F2.

rule4(X eq T, List, List, failure4) :-    % Fails if occur check succeeds.
  var(X),
  occur(X, T), !.                           

rule4(X eq T, List, NewList, modified) :-
  var(X),
  subst_list(X, T, List, NewList),
  List \== NewList.


%  occur(X, T) - X occurs in the term T.
%  occur_list(X, List) -
%    Apply occur(X) to all elements of the list,
%      succeed if the sublist of elements on which it succeeds is non-empty.

occur(X, T) :- X == T, !.       % T is X
occur(X, T) :-                  % Recurse on subterms
  nonvar(T),
  T =.. [_ | Subterms],
  occur_list(X, Subterms).

occur_list(X, List) :-
  sublist(occur(X), List, [_|_]).

%  subst_list(X, T, List, NewList)
%    Substitute T for X in all elements of List, result is NewList.
%  subst(X, T, Term, NewTerm)
%    Substitute T for X in Term, result is NewTerm.

subst_list(X, T, List, NewList) :-
  maplist(subst(X, T), List, NewList).

subst(X, T, Term, T) :- X == Term, !.   % If Term is X, return T.
subst(X, T, Term, NewTerm) :-           % Recurse on subterms.
  nonvar(Term),
  Term =.. [F | SubTerms], !,
  subst_list(X, T, SubTerms, NewSubTerms),
  NewTerm =.. [F | NewSubTerms].
subst(_, _, Term, Term).                % Otherwise, do nothing.
