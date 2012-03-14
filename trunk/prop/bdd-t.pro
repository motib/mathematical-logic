% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Test BDD algorithms.

user:file_search_path(common,'../common').
  :- ensure_loaded(common(def)).
  :- ensure_loaded(bdd).
  :- ensure_loaded(bddwrite).

% Test BDD apply by building BDDs from literals.

f(1) :-
  literal(pos, 1, P),
  literal(pos, 2, Q),
  literal(pos, 3, R),
  apply(Q, and, R, QandR),
  apply(P, or,  QandR, PorQandR),
  apply(P, or,  Q, PorQ),
  apply(P, or,  R, PorR),
  apply(PorQ, and, PorR, PorQandPorR),
  write_bdd(PorQandR),
  write_bdd(PorQandPorR).

f(2) :-
  literal(pos, 2, P),
  literal(pos, 3, Q),
  literal(pos, 1, R),
  apply(Q, and, R, QandR),
  apply(P, or,  QandR, PorQandR),
  apply(P, or,  Q, PorQ),
  apply(P, or,  R, PorR),
  apply(PorQ, and, PorR, PorQandPorR),
  write_bdd(PorQandR),
  write_bdd(PorQandPorR).

f(3) :-
  literal(pos, 1, P),
  literal(pos, 2, Q),
  literal(pos, 3, R),
  apply(Q, imp,  R, QimpR),
  apply(P, imp,  QimpR, PimpQR),
  apply(P, imp,  R, PimpR),
  apply(Q, imp,  PimpR, QimpPR),
  write_bdd(PimpQR),
  write_bdd(QimpPR).

f(4) :-
  literal(pos, 3, P),
  literal(pos, 2, Q),
  literal(pos, 1, R),
  apply(Q, imp,  R, QimpR),
  apply(P, imp,  QimpR, PimpQR),
  apply(P, imp,  R, PimpR),
  apply(Q, imp,  PimpR, QimpPR),
  write_bdd(PimpQR),
  write_bdd(QimpPR).

f(5) :-
  literal(pos, 1, P),
  literal(pos, 2, Q),
  apply(P, and,  Q, PandQ),
  apply(P, eqv,  Q, PeqvQ),
  apply(P, or,   Q, PorQ),
  apply(PeqvQ, eqv, PorQ, EqvOr),
  write_bdd(PandQ),
  write_bdd(EqvOr).

f(6) :-
  literal(pos, 1, P),
  literal(pos, 2, Q),
  apply(P, and,  Q, PandQ),
  apply(P, nand,  Q, PnandQ),
  apply(PnandQ, nand, PnandQ, NN),
  write_bdd(PandQ),
  write_bdd(NN).

f(7) :-
  literal(pos, 1, P),
  literal(pos, 2, Q),
  literal(neg, 2, NegQ),
  apply(NegQ, imp,  P, NegQimpP),
  apply(NegQimpP, imp,  Q, NQPQ),
  write_bdd(NQPQ).

f(8) :-
  literal(pos, 1, B1),
  literal(pos, 2, B2),
  literal(neg, 1, NotB1),
  literal(neg, 2, NotB2),
  apply(B1,    or, B2,    PosDisj),
  apply(NotB1, or, NotB2, NegDisj),
  apply(PosDisj, and, NegDisj, Sum1),
  apply(B1, xor, B2, Sum2),
  write_bdd(Sum1),
  write_bdd(Sum2).

f(9) :-
  literal(pos, 1, P),
  literal(pos, 2, Q),
  literal(pos, 3, R),
  apply(Q, or,  R, QorR),
  apply(P, and, QorR, PandQorR),
  apply(P, and, Q, PandQ),
  apply(P, and, R, PandR),
  apply(PandQ, or, PandR, PandQorPandR),
  write_bdd(PandQorR),
  write_bdd(PandQorPandR).

f(10) :-
  literal(pos, 3, P),
  literal(pos, 2, Q),
  literal(pos, 1, R),
  apply(Q, or,  R, QorR),
  apply(P, and, QorR, PandQorR),
  apply(P, and, Q, PandQ),
  apply(P, and, R, PandR),
  apply(PandQ, or, PandR, PandQorPandR),
  write_bdd(PandQorR),
  write_bdd(PandQorPandR).

f(11) :-
  literal(pos, 1, P),
  literal(pos, 2, Q),
  literal(pos, 3, R),
  apply(P, xor, Q, PxorQ),
  write_bdd(PxorQ),
  apply(P, xor, R, PxorR),
  write_bdd(PxorR),
  apply(PxorQ, xor, PxorR, PQR),
  write_bdd(PQR).

f(12) :-
  literal(pos, 1, P1),
  literal(pos, 2, P2),
  literal(pos, 3, P3),
  literal(pos, 4, P4),
  literal(pos, 5, P5),
  literal(pos, 6, P6),
  literal(pos, 7, P7),
  literal(pos, 8, P8),
  apply(P1, and, P2, P12),
  apply(P3, and, P4, P34),
  apply(P12, or, P34, R1),
  apply(P5, and, P6, P56),
  apply(P7, and, P8, P78),
  apply(P56, or, P78, R2),
  apply(R1, or, R2, R),
  write_bdd(R).

f(13) :-
  literal(pos, 1, P1),
  literal(pos, 5, P2),
  literal(pos, 2, P3),
  literal(pos, 6, P4),
  literal(pos, 3, P5),
  literal(pos, 7, P6),
  literal(pos, 4, P7),
  literal(pos, 8, P8),
  apply(P1, and, P2, P12),
  apply(P3, and, P4, P34),
  apply(P12, or, P34, R1),
  apply(P5, and, P6, P56),
  apply(P7, and, P8, P78),
  apply(P56, or, P78, R2),
  apply(R1, or, R2, R),
  write_bdd(R).

f(14) :-
  literal(pos, 1, A),
  literal(pos, 2, B),
  literal(pos, 3, C),
  literal(neg, 2, NB),
  apply(NB, and, A, NBCA),
  apply(B, and, C, BAC),
  apply(NBCA, or, BAC, One),
  write_bdd(One),
  apply(NB, imp, A, NBIA),
  apply(B, imp, C, BIC),
  apply(NBIA, and, BIC, Two),
  write_bdd(Two).

f(15) :-
  literal(pos, 1, A),
  literal(pos, 2, B),
  literal(pos, 3, C),
  apply(B, and, C,  BC),
  apply(A, eqv, BC, E),
  write_bdd(E).

f(16) :-
  literal(pos, 1, A),
  literal(pos, 2, B),
  literal(pos, 3, C),
  literal(neg, 2, NB),
  literal(neg, 3, NC),
  apply(B, and, C,  BC),
  apply(A, and, NB, ANB),
  apply(ANB, and, NC, ANBNC),
  apply(BC, or, ANBNC, F),
  write('Formula'), nl,
  write_bdd(F),
  restrict(F, 2, t, FT),
  write('Restricted to t'), nl,
  write_bdd(FT),
  restrict(F, 2, f, FF),
  write('Restricted to f'), nl,
  write_bdd(FF),
  apply(FT, or, FF, Exist),
  write('Exists B'), nl,
  write_bdd(Exist),
  exists(F, 2, E),
  write_bdd(E),
  exists(F, 2, E1),
  write_bdd(E1).

f(17) :-
  literal(pos, 1, A),
  literal(pos, 2, B),
  literal(pos, 3, C),
  apply(B, and, C,  BC),
  apply(A, or, BC, F),
  write('Formula'), nl,
  write_bdd(F),
  restrict(F, 3, t, FT),
  write('Restricted to t'), nl,
  write_bdd(FT),
  restrict(F, 3, f, FF),
  write('Restricted to f'), nl,
  write_bdd(FF),
  apply(FT, or, FF, Exist),
  write('Exists C'), nl,
  write_bdd(Exist),
  exists(F, 3, E),
  write_bdd(E).

f(18) :-
  literal(pos, 1, A),
  literal(pos, 2, B),
  literal(pos, 3, C),
  apply(B, and, C,  BC),
  apply(A, or, BC, F),
  write('Formula'), nl,
  write_bdd(F),
  restrict(F, 1, t, FT),
  write('Restricted to t'), nl,
  write_bdd(FT),
  nl,
  restrict(F, 1, f, FF),
  write('Restricted to f'), nl,
  write_bdd(FF),
  apply(FT, or, FF, Exist),
  write('Exists A'), nl,
  write_bdd(Exist),
  exists(F, 1, E),
  write_bdd(E).

f(19) :-
  literal(pos, 1, A),
  literal(pos, 2, B),
  literal(pos, 3, C),
  literal(neg, 2, NB),
  literal(neg, 3, NC),
  apply(B, and, C,  BC),
  apply(A, and, NB, ANB),
  apply(ANB, and, NC, ANBNC),
  apply(BC, or, ANBNC, F),
  write('Formula'), nl,
  write_bdd(F),
  restrict(F, 1, t, FT),
  write('Restricted to t'), nl,
  write_bdd(FT),
  restrict(F, 1, f, FF),
  write('Restricted to f'), nl,
  write_bdd(FF),
  apply(FT, or, FF, Exist),
  write('Exists A'), nl,
  write_bdd(Exist),
  exists(F, 1, E),
  write_bdd(E).


%  Database of (unreduced) bbd's for testing.
%
%    get_bdd(N, B)     - B is Nth bdd in database.
%    get_reduced(N, B) - as above, but reduce the bdd.   

get_reduced(N, B) :- get_bdd(N, B1), reduce(B1, B).

get_bdd(1,         % Bryant, 1992, Fig. 1-2.
  bdd(1,
    bdd(2,
      bdd(3, bdd(leaf, f, x), bdd(leaf, f, x)),
      bdd(3, bdd(leaf, f, x), bdd(leaf, t, x))
    ),
    bdd(2,
      bdd(3, bdd(leaf, f, x), bdd(leaf, t, x)),
      bdd(3, bdd(leaf, f, x), bdd(leaf, t, x))
    )
  )
).

get_bdd(2,         % not (v1 and v3) from Bryant, 1986, Fig. 7
  bdd(1,
    bdd(2,
      bdd(3, bdd(leaf, t, x), bdd(leaf, t, x)),
      bdd(3, bdd(leaf, t, x), bdd(leaf, t, x))
    ),
    bdd(2,
      bdd(3, bdd(leaf, t, x), bdd(leaf, f, x)),
      bdd(3, bdd(leaf, t, x), bdd(leaf, f, x))
    )
  )
).

get_bdd(3,         % v2 and v3 from Bryant, 1986, Fig. 7
  bdd(1,
    bdd(2,
      bdd(3, bdd(leaf, f, x), bdd(leaf, f, x)),
      bdd(3, bdd(leaf, f, x), bdd(leaf, t, x))
    ),
    bdd(2,
      bdd(3, bdd(leaf, f, x), bdd(leaf, f, x)),
      bdd(3, bdd(leaf, f, x), bdd(leaf, t, x))
    )
  )
).

get_bdd(4,         % ((v1 or v2) and v3) or v4 from Bryant, 1992, Fig. 6
  bdd(1,
    bdd(2,
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      ),
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, t, x), bdd(leaf, t, x))
      )
    ),
    bdd(2,
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, t, x), bdd(leaf, t, x))
      ),
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, t, x), bdd(leaf, t, x))
      )
    )
  )
).

get_bdd(5,         % (v1 and not v3) or v4 from Bryant, 1992, Fig. 6
  bdd(1,
    bdd(2,
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      ),
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      )
    ),
    bdd(2,
      bdd(3, 
        bdd(4, bdd(leaf, t, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      ),
      bdd(3, 
        bdd(4, bdd(leaf, t, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      )
    )
  )
).

get_bdd(6,         % (v2 and v3) or (v1 and not v2 and not v3)
  bdd(1,           %    from Bryant, 1992, Fig. 8
    bdd(2,
      bdd(3, bdd(leaf, f, x), bdd(leaf, f, x)),
      bdd(3, bdd(leaf, f, x), bdd(leaf, t, x))
    ),
    bdd(2,
      bdd(3, bdd(leaf, t, x), bdd(leaf, f, x)),
      bdd(3, bdd(leaf, f, x), bdd(leaf, t, x))
    )
  )
).

get_bdd(7,         % v1 v v2
  bdd(1,
    bdd(2, bdd(leaf, f, x), bdd(leaf, t, x)),
    bdd(2, bdd(leaf, t, x), bdd(leaf, t, x))
  )
).

get_bdd(8,         % ~v1 v ~v2
  bdd(1,
    bdd(2, bdd(leaf, t, x), bdd(leaf, t, x)),
    bdd(2, bdd(leaf, t, x), bdd(leaf, f, x))
  )
).

get_bdd(9,         % v1 xor v2
  bdd(1,
    bdd(2, bdd(leaf, f, x), bdd(leaf, t, x)),
    bdd(2, bdd(leaf, t, x), bdd(leaf, f, x))
  )
).

get_bdd(10,
  bdd(1,
    bdd(2,
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, f, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      ),
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, f, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      )
    ),
    bdd(2,
      bdd(3, 
        bdd(4, bdd(leaf, f, x), bdd(leaf, f, x)),
        bdd(4, bdd(leaf, f, x), bdd(leaf, t, x))
      ),
      bdd(3, 
        bdd(4, bdd(leaf, t, x), bdd(leaf, t, x)),
        bdd(4, bdd(leaf, t, x), bdd(leaf, t, x))
      )
    )
  )
).

get_bdd(11,         % v1 imp v2
  bdd(1,
    bdd(2, bdd(leaf, t, x), bdd(leaf, t, x)),
    bdd(2, bdd(leaf, f, x), bdd(leaf, t, x))
  )
).

get_bdd(12,         % v1 imp v2
  bdd(2,
    bdd(1, bdd(leaf, t, x), bdd(leaf, f, x)),
    bdd(1, bdd(leaf, t, x), bdd(leaf, t, x))
  )
).

%  Test programs
%    tall runs everything, writing output to 'tall.txt'.
%
%    tapp86/92(Opr) - Apply operation from Bryant's examples.
%    tred(N)        - Reduce of N'th BDD in above database.
%    tres(N, K, V)  - Restrict N'th BDD to Value V for variable K.
%    tadd           - Verify the sum of a one-bit adder:
%                     B1 xor B2 = (B1 or B2) and (not B1 or not B2)
%    f(N)           - Create BDDs by applying operations to literals.

tall :- tell('tall.txt'),                        fail.
tall :- write('Reduce...'),     nl, tred(_), nl, fail.
tall :- write('Apply 86...'),   nl, tapp86(or),  fail.
tall :- write('Apply 92...'),   nl, tapp92(or),  fail.
tall :- write('Adder...'),      nl, tadd,        fail.
tall :- write('Restrict v2 to '),
        value(V), write(V),     nl, tres(_,2,V), nl, fail.
tall :- write('Create...'),     nl, f(_),    nl, fail.
tall :- told.

value(t).
value(f).

tapp86(Opr) :-
  get_reduced(2, B1), write_bdd(B1),
  get_reduced(3, B2), write_bdd(B2),
  apply(B1, Opr, B2, Result),
  write('Result is'), nl,
  write_bdd(Result).

tapp92(Opr) :-
  get_reduced(4, B1), write_bdd(B1),
  get_reduced(5, B2), write_bdd(B2),
  apply(B1, Opr, B2, Result),
  write('Result is'), nl,
  write_bdd(Result).

tred(N) :-
  get_bdd(N, B),
  write('Reduce '), write(N), nl,
  reduce(B,A),
  write_bdd(A).

tres(N, K, V) :-
  get_reduced(N, A),
  write('Restrict '), write(N), nl,
  write_bdd(A),
  restrict(A, K, V, A1),
  write_bdd(A1).

tadd :-
  literal(pos, 1, B1),
  literal(pos, 2, B2),
  literal(neg, 1, NotB1),
  literal(neg, 2, NotB2),
  apply(B1,    or, B2,    PosDisj),
  apply(NotB1, or, NotB2, NegDisj),
  apply(PosDisj, and, NegDisj, Sum1),
  apply(B1, xor, B2, Sum2),
  write_bdd(Sum1),
  write_bdd(Sum2),
  (Sum1 = Sum2 -> write('Equal') ; write('Not equal')),
  nl.
