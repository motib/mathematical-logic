% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  module def
%    semantic definitions of Boolean operators

:- module(def, [opr/4, negate/2]).

%    opr for binary operators, negate for negation

opr(or,  t,t,t). opr(or,  t,f,t). opr(or,  f,t,t). opr(or,  f,f,f).
opr(and, t,t,t). opr(and, t,f,f). opr(and, f,t,f). opr(and, f,f,f).
opr(xor, t,t,f). opr(xor, t,f,t). opr(xor, f,t,t). opr(xor, f,f,f).
opr(eqv, t,t,t). opr(eqv, t,f,f). opr(eqv, f,t,f). opr(eqv, f,f,t).
opr(imp, t,t,t). opr(imp, t,f,f). opr(imp, f,t,t). opr(imp, f,f,t).
opr(nor, t,t,f). opr(nor, t,f,f). opr(nor, f,t,f). opr(nor, f,f,t).
opr(nand,t,t,f). opr(nand,t,f,t). opr(nand,f,t,t). opr(nand,f,f,t).

negate(t,f).
negate(f,t).
