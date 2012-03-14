% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Write a BDD.

:- module(bddwrite,
     [write_bdd/1,
      write_bdd/2]).

%  The nodes are numbers by IDs.
%  id is used to generate node IDs.
%  bddid caches pairs (bdd, id):
%    if a subBDD appears again, write the previous ID

:- dynamic id/1, bddid/2.


%  write_bdd(B)         - write a bdd after clearing database.
%    (1) If B has been encountered just write the node id.
%    (2) For terminals, write the value, assign id and cache the node.
%    (3) For nonterminals, write the variable number,
%          assign id, cache the node and recurse.
%
%  write_bdd(B, Write) 
%    The Write argument is a predicate used to write the variable N.
%    write_bdd/1 calls write_bdd/2 with default predicate write_atom 
%
%  write_bdd(B, indent, Write) - auxiliary predicate with indent count.

write_bdd(B)  :-
  write_bdd(B, write_atom).

write_bdd(B, Write) :-
  retractall(bddid(_,_)),
  retractall(id(_)),
  write_bdd(B, 0, Write).

write_bdd(B, Indent, _) :- 
  bddid(B, ID), !, 
  tab(4+Indent),
  write_node(ID).

write_bdd(B, Indent, _) :-
  B = bdd(leaf, Val, _), !,
  generate_id(ID), 
  assert(bddid(B, ID)),
  write_node(ID),
  tab(Indent),
  write(Val).

write_bdd(B, Indent, Write) :-
  B = bdd(N, False, True),
  generate_id(ID),
  assert(bddid(B, ID)),
  write_node(ID),
  tab(Indent),
  call(Write,N), nl,
  Indent1 is Indent + 3,
  write_bdd(False,  Indent1, Write), nl,
  write_bdd(True,   Indent1, Write), nl.

%  Default is write a variable as vN.

write_atom(N) :- write('v'), write(N).

%  write_node(N)        - write node id N.

write_node(ID) :-
  width(ID), write('['), write(ID), write('] ').

%  width(ID)            - write ID in field of width 3.

width(ID) :- ID < 100, !, write(' ').
width(ID) :- ID <  10, !, write(' ').
width(_).

%  generate_id(id(ID))  - increment the id.

generate_id(ID) :-
  retract(id(N)), !, ID is N + 1, assert(id(ID)).
generate_id(1)  :-
  assert(id(1)).
