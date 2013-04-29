% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Configuration
%    version, default algorithm mode, default display options
%    dot prologue and decorations

:- module(config,
  [version/1, years/1,
   default_alg_mode/1, default_display/1, default_learn_mode/1,
   dot_prologue/2, dot_decorate/2]).

version('1.3.4').

years('2012-13').

default_alg_mode(dpll).

default_learn_mode(resolution).

default_display(
  [backtrack, conflict, decision, learned,
  resolvent, result, skipped, sorted, uip, unit]).

% dot prologue:
%   lr - left to right for implication graphs
%   tb - top to bottom for trees of assignments
dot_prologue(lr, 'digraph G {\n  rankdir=LR;\n').
dot_prologue(tb, 'digraph G {\n  rankdir=TB ranksep=equally;\n').

% dot decorations
%   decision: decision assignment in tree and implication graph
%   decision_level: decision assignments with graph with dominator
%   dominator: dominator node and the kappa node
%   conflict: conflict leaf in tree
%   sat: leaf for satisfying assignment in a tree
dot_decorate(decision,       ' [color="red"]').
dot_decorate(decision_level, ' [color="red"   peripheries="2"]').
dot_decorate(dominator,      ' [peripheries="2"]').
dot_decorate(conflict,       ' [color="red"   peripheries="2"]').
dot_decorate(sat,            ' [color="green" peripheries="2"]').

% Alternate decoration that doesn't use colors
% dot_decorate(decision,       ' [style="bold"]').
% dot_decorate(decision_level, ' [color="bold" peripheries="2"]').
% dot_decorate(dominator,      ' [style="bold" peripheries="2"]').
% dot_decorate(conflict,       ' [style="bold" peripheries="2"]').
% dot_decorate(sat,            ' [style="bold" peripheries="3"]').
