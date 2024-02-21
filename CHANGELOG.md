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
* Extended collection of statistics to include conflict size, learned clause length and backtrack amount
* Added logging of statistics using `info!` when using the `OptimisationSolver` 
* Refactored the CP and propositional trails, and extracted a trail abstraction.
* Added a deterministic hashing function for reproducibility + added pre-commit hook to detect hash-based structure usage from std
* Added explicit support for constraints as an abstraction over propagators. The 
  following constraints are provided (also implemented for the FlatZinc compiler):
  * int_lin_ne
  * int_lin_le
  * int_lin_le_reif
  * int_lin_eq
  * int_lin_eq_reif
  * int_ne
  * int_le
  * int_le_reif
  * int_lt
  * int_lt_reif
  * int_eq
  * int_eq_reif
  * int_plus
  * int_times
  * int_max
  * int_min
  * int_array_maximum
  * int_array_minimum
  * array_bool_and
  * array_bool_or
  * array_bool_element
  * array_var_bool_element
  * bool2int
  * bool_and
  * bool_clause
  * bool_eq
  * bool_eq_reif
  * bool_le
  * bool_le_reif
  * bool_lt
  * bool_lt_reif
  * bool_not
  * bool_or
  * all_different
  * set_in_reif
* Add support for variables with negative domain bounds.
* Added eager explanations to CP, lazy is still possible by passing a closure.
* Added KeyedVec structure which ensures that structures can only be indexed by one type of key
* Add support for optimization functions MiniZinc

## Bugfixes

* When backtracking, the CP view of individual domains did not correctly update 
state to indicate the bounds were in the domain.
* The OptimisationSolver did not have the correct exit conditions nor was it bounding the literals correctly when creating the encoder if literals were assigned at the root level

## Refactors
* Added a variable to the watchlists to prevent traversing the literal and/or integer trail when no variable is being watched
* The creation of `AssignmentsInteger` for debug purposes did not properly create the `EventSink` leading to out-of-bounds errors + one of the debug checks failed when a predicate which was always true/false was added
* Set C++ -std language version to make sure integration test tools build properly on MacOS
* Prevent unnecessary encoding from taking place when the objective function is a single integer variable
