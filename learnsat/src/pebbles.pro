% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

:- use_module(dpll).

%  Grid pebbling

grid2 :-
  dpll(
  [
    % target clauses
    [~p1], [~p2],      
    % precedence clauses
    [~q1, ~r1, p1, p2],   
    [~q1, ~r2, p1, p2],
    [~q2, ~r1, p1, p2],
    [~q2, ~r2, p1, p2],
     % source clauses
    [q1, q2], [r1, r2]
    
  ], _).

%  In dpll mode, this takes forever!
%  dpll: units=1135104, choices=477344, conflicts=425024
%  cdcl: units=117, choices=80, conflicts=76
%  ncb:  units=53, choices=29, conflicts=22

grid3 :-
  dpll(
  [
    % target clauses
    [~p1], [~p2],
    % precedence clauses
    [~q1, ~r1, p1, p2],
    [~q1, ~r2, p1, p2],
    [~q2, ~r1, p1, p2],
    [~q2, ~r2, p1, p2],
    [~s1, ~t1, q1, q2],
    [~s1, ~t2, q1, q2],
    [~s2, ~t1, q1, q2],
    [~s2, ~t2, q1, q2],
    [~t1, ~u1, r1, r2],
    [~t1, ~u2, r1, r2],
    [~t2, ~u1, r1, r2],
    [~t2, ~u2, r1, r2],
    % source clauses
    [s1, s2], [t1, t2], [u1, u2]
  ], _).
