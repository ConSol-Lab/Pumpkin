# DRCP Format

The Deletion Reverse Constraint Propagation format describes how a constraint programming solver proves unsatisfiability or optimality. This is a Rust library that provides a reader and writer of DRCP proof files, as well as the accompanying literal mapping file.

## Proof Format

In a DRCP proof, the smallest building block is an _atomic constraint_, which describes a fact about the domain of a single variable. An atomic constraint has the form `[x <op> v]`, where `x` is an integer variable, `<op>` is one of `==, !=, <=, >=`, and `v` is an integer constant. In a DRCP proof, the proof uses integer identifiers (which we will refer to as literals) to refer to atomic constraints. A new literal is introduced with the following line:

```
a <non-zero unsigned integer> [<variable> <op> <value>]

e.g.
a 1 [x1 >= 5]
a 3 [some_variable <= 5]
```

**Note:** Identifiers should follow the common identifier regex `[a-zA-Z_][a-zA-Z0-9_]*`. Essentially, an identifier can be an alphanumeric character or '_', but the first character cannot be a number.

Atomic constraints are used in the following proof steps:

### Inference
An inference step encodes the propagation of an atomic constraint. The inference step has the following format:
```
i <step_id> <premises> [0 <propagated>] [c:<constraint tag>] [l:<filtering algorithm>]
```

The individual components:
  - `<step_id>`: A non-zero integer which serves as a unique identifier for the step in the proof.
  - `<premises>` A space-separated list of atomic constraint identifiers.
  - `<propagated>` A single atomic constraint identifier.
  - `c:<constraint tag>`: _Optional_. A hint identifying the constraint that triggered the inference. This refers either to a constraint from the model or a previously derived nogood.
  - `l:<filtering algorithm>`: _Optional_. A hint specifying which filtering algorithm identified the inference.

If there is no `<propagated>`, then the inference reads that the premises imply false.

### Nogood
A nogood step encodes a partial assignment which cannot be extended to a solution.
```
n <step_id> <atomic constraint ids> [0 <propagation hint>]
```

The individual components:
  - `<step_id>`: A non-zero integer which serves as a unique identifier for the step in the proof.
  - `<atomic constraint ids>` A space-separated list of atomic constraint identifiers. These encode the nogood _as an implication that implies false_.
  - `0 <propagation hint>`: _Optional_. A hint which is a separated list of step ids, noting what order the steps can be applied to derive this nogood.

### Conclusion
The conclusion finishes the proof. It is either the claim that the problem is unsatisfiable:
```
c UNSAT
```

Or it is the claim of a dual bound for the objective variable:
```
c <objective bound>
```
where `<objective bound>` is an atomic constraint id encoding the dual bound on the objective variable.
