% Copyright 2012-13 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Configuration
%    version, default algorithm mode, default display options
%    dot prologue and decorations

:- module(config,
  [version/1, years/1, default_mode/1, default_display/1,
   dot_prologue/1, dot_decorate/1]).

version('1.3.0').

years('2012-13').

default_mode(dpll).

default_display(
  [backtrack, conflict, decision, learned,
  resolvent, result, skipped, uip, unit]).

dot_prologue('digraph G {\n  rankdir=LR;\n').

dot_decorate(' [color="red"];\n').

%dot_decorate(' [style="bold"];\n').
