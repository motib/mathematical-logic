% Copyright 2012 by M. Ben-Ari. GNU GPL. See copyright.txt.

% Regression testing:
%   run all the examples with all display options set (except "dot"). 

:- ensure_loaded([examples,pigeon,queens,tseitin]).

all :-
  set_mode(ncb),
  set_display(all),
  clear_display(dot),
  tell('all.txt'),
  mlm,
  mz,
  ms,
  hole2,
  hole3,
  queens4,
  ex,
  exs,
  k22,
  k22s,
  k33,
  k33s,
  told.
