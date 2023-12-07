# Version [next]

## Features

* Added Element CP constraint and propagator (arc-consistent)
* Added the LinearNe propagator (arc-consistent)
* Added parsing of the following FlatZinc rules:
  * array of int parameters
  * array of int variables
  * int variables with domains in the form <int-literal>..<int-literal>.
  * the linear_ne constraint
  * the 'satisfy' goal
* Started supporting solution logging according to the FlatZinc specification

## Bugfixes

* When backtracking, the CP view of individual domains did not correctly update 
state to indicate the bounds were in the domain.
* The OptimisationSolver did not have the correct exit conditions nor was it bounding the literals correctly when creating the encoder if literals were assigned at the root level
