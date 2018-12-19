# Mathematical Logic for Computer Science

**Mordechai (Moti) Ben-Ari**

[http://www.weizmann.ac.il/sci-tea/benari/](http://www.weizmann.ac.il/sci-tea/benari/)

---

The repository contains programs implementing algorithms from my textbook *Mathematical Logic for Computer Science*, Springer, 2012, ISBN 978-1-4471-4128-0.

The programs are written in Prolog and were run using SWI Prolog.

## Licence

Copyright 2002--2018 by Moti Ben-Ari under the GNU GPL license.

## Materials

### Documentation

The programs are documented in mlcs-programs-documentation.tex from which the pdf file was derived.

### Directory sturcture

* common - common modules and files used by other programs
* prop   - propositional calculus
* bdd    - binary decision diagrams
* fol    - first order logic
* lp     - logic programming
* tl     - temporal logic

For each program p.pro, there is a file p-t.pro which contains test programs. The programs use the predicate file_search_path to access the common modules in the common directory, so maintain the relative directory structure.

### List of files:

Directory common:

* ops.pro     - declaration of operators with precedence and associativity.
* def.pro     - correspondance between symbols and internal operators.
              - semantic definition of Boolean operators.
* intext.pro  - conversion from external to internal format and conversely.
* io.pro      - display procedures for all programs (except BDDs).
* cnfpro.pro  - conversion of a formula to CNF

Directory prop:   (propositional calculus)
* tt.pro      - truth tables.
* tabl.pro    - semantic tableaux.
* systab.pro  - systematic semantic tableaux.
              - note: tabl and systab share a test program tabl-t.pl.
* check.pro   - Hilbert proof checker.
* resolv.pro  - resolution.
* bdd.pro     - binary decision diagrams

Directory fol: first order logic

* tabl.pro    - semantic tableaux.
* check.pro   - Hilbert proof checker.
* cnffol.pro  - conver to CNF
* skolem.pro  - skolemize a formula.
* unify.pro   - unification algorithm.
* resolv.pro  - resolution.
* utility.pro - procedures common to more than one file.

Directory tl:     (temporal logic)

* tl.pro      - semantic tableaux .
* peter.pro   - state diagram for Peterson's algorithm.
* sym.pro     - symbolic model checking.
