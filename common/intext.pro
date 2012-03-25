% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Module intext
%    translates internal to external symbols and conversely

:- module(intext,
  [symbol_opr/2,
   latex_opr/2,
   to_external/2,
   to_external_list/2,
   to_internal/2,
   to_internal_list/2,
   transform/2]).

%  Translation between operator symbols

symbol_opr(+ ,   xor).
symbol_opr(<-> , eqv).
symbol_opr(--> , imp).
symbol_opr(v ,   or).
symbol_opr(^ ,   and).
symbol_opr(~ ,   neg).
symbol_opr(# ,   always).
symbol_opr(<> ,  eventually).
symbol_opr(@,    next).

latex_opr('\\oplus',   xor).
latex_opr('\\leftrightarrow', eqv).
latex_opr('\\rightarrow', imp).
latex_opr('\\vee',   or).
latex_opr('\\wedge',   and).
latex_opr('\\neg',   neg).
latex_opr('\\Box',   always).
latex_opr('\\Diamond',  eventually).
latex_opr('\\ocircle',    next).
latex_opr('\\forall',    'forall').
latex_opr('\\exists',   'exists').

%  to_external(Int, Ext) -
%    translates Int in internal format to Ext in external format

%  Quantifiers
to_external(ex(X,Fml),  ex(X,EFml))  :- !,
  to_external(Fml, EFml). 
to_external(all(X,Fml), all(X,EFml)) :- !,
  to_external(Fml, EFml).

%  Binary operators
to_external(Fml, Ext) :-
  Fml =.. [Opr, F1, F2],       % decompose binary formula
  symbol_opr(Sym, Opr), !,     % get symbol
  to_external(F1, E1),         % recurse on subformulas
  to_external(F2, E2),
  Ext =.. [Sym, E1, E2].       % recompose formulas

% Unary operators
to_external(Fml, Ext) :-
  Fml =.. [Opr , F],
  symbol_opr(Sym, Opr), !,
  to_external(F, E),
  Ext =.. [Sym, E].

to_external(F, F).

%  to_internal(Ext, Int) -
%    translates Ext in external format to  Int in internal format

%  Quantifiers
to_internal(ex(X,Fml),  ex(X,IFml)) :- !,
  to_internal(Fml, IFml).
to_internal(all(X,Fml), all(X,IFml)) :- !,
  to_internal(Fml, IFml).

%  Binary operators
to_internal(Fml, Int) :-
  Fml =.. [Sym, F1, F2],      % decompose binary formula
  symbol_opr(Sym, Opr), !,    % get operator
  to_internal(F1, I1),        % recurse on subformulas
  to_internal(F2, I2),
  Int =.. [Opr, I1, I2].      % recompose formula

% Unary operators
to_internal(Fml, Int) :-
  Fml =.. [Sym, F],           % similarly for unary formula
  symbol_opr(Sym, Opr), !,
  to_internal(F, I),
  Int =.. [Opr, I].

to_internal(F, F).

%  to_external_list(IntList, ExtList)
%  to_internal_list(ExtList, IntList)
%    apply to_external/to_internal to lists

to_external_list([], []).
to_external_list([H|T], [HI|TI]) :-
  to_external(H, HI),
  to_external_list(T, TI).

to_internal_list([], []).
to_internal_list([H|T], [HI|TI]) :-
  to_internal(H, HI),
  to_internal_list(T, TI).

%  transform(L1, L2)
%    L1 is a list of deductions.
%    L2 has all formulas transformed to internal format.

transform([],[]).
transform([deduce(L, F) | Tail], [deduce(LI, FI) | ITail]) :-
  to_internal(F, FI),
  to_internal_list(L, LI),
  transform(Tail, ITail).
