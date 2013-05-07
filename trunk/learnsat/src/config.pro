% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Configuration
%    version, default algorithm mode, default display options
%    dot prologue, decorations and decorate mode

:- module(config,
  [version/1, years/1,
   default_alg_mode/1, default_display/1, dot_prologue/2,
   dot_decorate/2, decorate_mode/1]).

version('1.4.1').

years('2012-13').

default_alg_mode(dpll).

default_display(
  [backtrack, conflict, decision, learned,
  resolvent, result, skipped, sorted, uip, unit]).

decorate_mode(color).
%decorate_mode(bw).


% dot prologue:
%   lr - left to right for implication graphs
%   tb - top to bottom for trees of assignments
dot_prologue(lr, 'digraph G {\n  rankdir=LR;\n').
dot_prologue(tb, 'digraph G {\n  rankdir=TB ranksep=equally;\n').

% dot decorations
%   There are versions for color and black-and-white

%   decision: decision assignment in tree and implication graph
%   decision_level: decision assignments with graph with dominator
%   dominator: dominator node and the kappa node
%   conflict: conflict leaf in tree
%   sat: leaf for satisfying assignment in a tree
%   cut: edge that is part of a cut

dot_decorate(What, Decoration) :-
  decorate_mode(color), !,
  dot_decorate(color, What, Decoration).
dot_decorate(What, Decoration) :-
  dot_decorate(bw, What, Decoration).

dot_decorate(color, decision,       ' [color="red"]').
dot_decorate(color, decision_level, ' [color="red"   peripheries="2"]').
dot_decorate(color, dominator,      ' [peripheries="2"]').
dot_decorate(color, conflict,       ' [color="red"   peripheries="2"]').
dot_decorate(color, sat,            ' [color="green" peripheries="2"]').
dot_decorate(color, cut,            ' color="blue"').

dot_decorate(bw, decision,       ' [style="bold"]').
dot_decorate(bw, decision_level, ' [color="bold" peripheries="2"]').
dot_decorate(bw, dominator,      ' [style="bold" peripheries="2"]').
dot_decorate(bw, conflict,       ' [style="bold" peripheries="2"]').
dot_decorate(bw, sat,            ' [style="bold" peripheries="3"]').
dot_decorate(bw, cut,            ' style="dashed"').
