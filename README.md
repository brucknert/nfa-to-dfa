Simple Haskell program that takes NFA (Nondetermistic Finite Automata) and generates equivalent minimal DFA (Determistic Finite Automata).

More information: https://en.wikipedia.org/wiki/DFA_minimization

Input:

First line specifies all the states of the NFA separated by comma.

Second line specifies initial state.

Third line specifies finite states separated by comma.
Any additional line specifies transitions.
For transition from state q1 to state q2 using symbol a - **q1,a,q2**.
For transition from state q1 to state q2 with epsilon transition - **q1,,q2**. See example input files in */example*.

Compile:
gmake

Compile and run tests:
gmake run

Compile and run program with input from stdin:
gmake runstdin

Generate haddock documentation:
gmake doc
