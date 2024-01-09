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
* Added support for propagating literals in CP propagators.
* Implemented reified linear leq propagator.
* Added disjunctive scheduling example through a difference logic model.
* Refactored the CP and propositional trails, and extracted a trail abstraction.
* Added explicit support for constraints as an abstraction over propagators. The 
  following constraints are provided:
  * int_lin_ne
  * int_lin_le
  * int_lin_le_reif
  * int_ne
  * int_times
  * all_different
* Add support for variables with negative domain bounds.

## Bugfixes

* When backtracking, the CP view of individual domains did not correctly update 
state to indicate the bounds were in the domain.
* The OptimisationSolver did not have the correct exit conditions nor was it bounding the literals correctly when creating the encoder if literals were assigned at the root level
