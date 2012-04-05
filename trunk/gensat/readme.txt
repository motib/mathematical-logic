                     GenerateSat

Copyright 2010 by Moti Ben-Ari. This work is licensed under the Creative
Commons Attribution-ShareAlike 3.0 License. To view a copy of this
license, visit \url{http://creativecommons.org/licenses/by-sa/3.0/}; or,
(b) send a letter to Creative Commons, 543 Howard Street, 5th Floor, San
Francisco, California, 94105, USA.

The program is copyright under the GNU General Public License.

GenerateSat is a Java program that generates propositional formulas as
sets of Tseitin clauses on the complete bipartite graph Kn,n. For an
explanation of the construction of Tseitin clauses, see Section 4.4 of
M. Ben-Ari, Mathematical Logic for Computer Science, Springer, 2001.
A formula is written as a Promela program for verification with Spin.
Here is the program that is generated for K2,2:

active proctype sat() {
	bool p0, p1, p2, p3;
	bool result;

	if :: p0 = true :: p0 = false fi;
	if :: p1 = true :: p1 = false fi;
	if :: p2 = true :: p2 = false fi;
	if :: p3 = true :: p3 = false fi;

	result =
	(!p0 || !p1) && ( p0 ||  p1) && (!p2 ||  p3) && ( p2 || !p3) &&
	(!p0 ||  p2) && ( p0 || !p2) && (!p1 ||  p3) && ( p1 || !p3) &&
	true;

	printf("p0 p1 p2 p3 \n");
	printf(" %d  %d  %d  %d  \n", p0, p1, p2, p3);
	printf("Result = %d\n", result);
	assert(!result);
}

Nondeterministic guarded commands choose an assignment to the atomic
propositions. Then the set of clauses is evaluated. If it evaluates to
false, no error is report, so a successful verification demonstrates
that the formula is unsatisfiable. 

In addition to the unsatisfiable formula, GenerateSat can generate
a number of satisfiable formulas by complementing a random literal. When
a satisfying interpretation is found, the assert(!result) causes an
error to terminate the verification. A guided simulation can then print
the assignment.

To run a verification use the following commands:
  spin -a [filename]
  gcc -o pan -DSAFETY pan.c
  pan -w25
where -w25 is the size of the hash table and can be changed as needed.

For the satisfiable sets of clauses, running several verifications in
parallel with search diversity can find the satisfying interpretation
more quickly. The search diversity is achieved by requesting a random
order of exploration of transitions and using different seeds for the
random number generator.

  spin -a [filename]
  gcc -o pan -DSAFETY -DT_RAND pan.c
  pan -w25 -RS[seed]
 
GenerateSat also generates batch files for these verifications.

Usage:

  GenerateSat n [k [r]]\n" +
    n is the size of the graph Kn,n
    k is the number of satisfiable formulas generated
    r is the number of random verifications for each such formula

The generated files are named satn-i.pml, where i is 0 for the
unsatisfiable formula and 1..k for the satisfiable formulas.

This archive contains the files satn-0.pml for n=2..6.
