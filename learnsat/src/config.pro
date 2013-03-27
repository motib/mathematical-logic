% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Configuration
%    version, default algorithm mode, default display options
%    dot prologue and decorations

:- module(config,
  [version/1, years/1, default_mode/1, default_display/1,
   dot_prologue/2, dot_decorate/2]).

version('1.3.1').

years('2012-13').

default_mode(dpll).

default_display(
  [backtrack, conflict, decision, learned,
  resolvent, result, skipped, sorted, uip, unit]).

% dot prologue:
%   lr - left to right for implication graphs
%   tb - top to bottom for semantic trees
dot_prologue(lr, 'digraph G {\n  rankdir=LR;\n').
dot_prologue(tb, 'digraph G {\n  rankdir=TB ranksep=equally;\n').

% dot decorations for the semantic trees
%   decision is also used for implication graphs
dot_decorate(decision, ' [color="red"]').
dot_decorate(conflict, ' [color="red" peripheries="2"]').
dot_decorate(sat,      ' [color="green" peripheries="2"]').
