% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

% Regression testing:
%   run the examples (except Pigeon Hole 3 and Tseitin K3,3
%   with all display options set except
%    'evaluate' (which is trivial) and 'dot' (the graph is not needed) 

:- ensure_loaded([examples,pigeon,queens,tseitin]).

all :-
  set_mode(ncb),
  set_display(all),
  clear_display([evaluate,dot]),
  tell('all.txt'),
  mlm,
  mz,
  mss,
  hole2,
% hole3,
  queens4,
  ex,
  exs,
  k22,
  k22s,
% k33,
  k33s,
  told.
