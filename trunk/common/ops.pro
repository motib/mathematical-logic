% Copyright 2000-2012 by M. Ben-Ari. GNU GPL. See prolog.pdf.

%  Logical operators

%  Alphabetic operators for internal use

:- op(650, xfy, xor).        /* exclusive or */
:- op(650, xfy, eqv).        /* equivalence  */ 
:- op(650, xfy, nor).        /* nor          */
:- op(650, xfy, nand).       /* nand         */ 
:- op(640, xfy, imp).        /* implication  */ 
:- op(630, xfy, or).         /* disjunction  */ 
:- op(620, xfy, and).        /* conjunction  */ 
:- op(610, fy,  neg).        /* negation     */
:- op(600, xfy, eq).         /* equality     */
:- op(610, fy,  always).     /* always       */ 
:- op(610, fy,  eventually). /* eventually   */ 
:- op(610, fy,  next).       /* next         */ 

%  Symbolic operators for external use

:- op(650, xfy, +).          /* exclusive or */
:- op(650, xfy, <->).        /* equivalence  */ 
:- op(640, xfy, -->).        /* implication  */ 
:- op(630, xfy, v).          /* disjunction  */ 
:- op(620, xfy, ^).          /* conjunction  */ 
:- op(610, fy,  ~).          /* negation     */ 
:- op(610, fy,  #).          /* always       */ 
:- op(610, fy,  <>).         /* eventually   */ 
:- op(610, fy,  @).          /* next         */ 
