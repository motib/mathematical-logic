## Programs for Mathematical Logic for Computer Science ##

These programs accompany the Third Edition of my textbook [Mathematical Logic for Computer Science](http://www.springer.com/978-1-4471-4128-0) that was published in 2012 by Springer. They are written in Prolog and were developed using the [SWI](http://www.swi-prolog.org/) implementation. The documentation is written in LaTeX and formatted to PDF.

As a courtesy, the last version of the programs for the Second Edition can be downloaded. However, I will not be supporting them nor responding the queries.

## LearnSAT ##

A [SAT solver](http://www.satlive.org/) is a program for searching for a satisfying interpretation of a formula in propositional logic. Although the SAT problem is NP-complete, in real-world examples modern SAT solvers have proven to be extremely efficient and are widely used in many fields such as artificial intelligence and hardware verification. For an overview of SAT solving, see Chapter 6 of the Third Edition of my textbook. [Handbook of Satisfiability](http://www.iospress.nl/book/handbook-of-satisfiability/) is the definitive reference for SAT solving.

LearnSAT is a program designed to facilitate learning the central algorithms used by modern SAT solvers: the _DPLL algorithm_ together with _conflict-directed clause-learning_ and _non-chronological backtracking_. LearnSAT is written in Prolog and the core algorithms consist of less than 200 lines of code. A very detailed trace of the algorithm's execution is displayed and the specific content of the trace can be set by the user. LearnSAT generates DOT files for the implication graphs and the tree of assignments.

The distribution includes a document with a user's guide and documentation of the software.

A tutorial on SAT solving with LearnSAT can be downloaded separately.


## Generating SAT instances for Spin ##

Gensat is a Java program that generates propositional formulas as sets of Tseitin clauses on the complete bipartite graph Kn,n. The formulas are written as Promela programs for verification with Spin. Promela programs for K2,2, K3,3, K4,4, K5,5, K6,6 are included. The use of these program is described in
[Mordechai (Moti) Ben-Ari and Fatima Kaloti-Hallak. Demonstrating random and parallel algorithms with Spin. ACM Inroads 3, 3 (September 2012), 36-38](http://doi.acm.org/10.1145/2339055.2339069).

A version for the [Erigone](http://code.google.com/p/erigone/) model checker is available as part of that project.

For an explanation of the construction of Tseitin clauses, see Section 4.5 of _Mathematical Logic for Computer Science (Third Edition)_.

Version 1.1 includes a version of Gensat for generating Tseitin clauses in Prolog for LearnSAT.