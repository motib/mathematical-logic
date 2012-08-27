% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

%  Configuration
%    version, default algorithm mode, default display options

:- module(config, [version/1, default_mode/1, default_display/1]).

version('1.0.4').

default_mode(dpll).

default_display([backtrack, conflict, decision, learned, result, unit]).
