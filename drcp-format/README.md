# DRCP Format

The Deletion Reverse Constraint Propagation format describes how a constraint programming solver proves unsatisfiability or optimality. This is a Rust library which provides a reader and writer of DRCP proof files, as well as the accompanying literal mapping file.

## Proof Format

In a DRCP proof, the smallest building block is an _atomic constraint_, which describes a fact about the domain of a single variable. An atomic constraint has the form `[x <op> v]`, where `x` is an integer variable, `<op>` is one of `==, !=, <=, >=`, and `v` is an integer constant. In a DRCP proof, the proof uses integer identifiers to refer to atomic constraints. A mapping of identifiers to atomic constraints is a `.lits` file, and looks like this:

```
1 [x1 >= 1]
2 [x2 <= 2]
```

Each line starts with a non-zero integer which is the identifier, then a space, and then the atomic constraint.

Atomic constraints are used in the following proof steps:

### Inference
An inference step encodes the propagation of an atomic constraint. The inference step has the following format:
```
i <step_id> <premises> 0 <propagated> [c:<constraint tag>] [l:<filtering algorithm>]
```

The individual components:
  - `<step_id>`: A non-zero integer which serves as a unique identifier for the step in the proof.
  - `<premises>` A space-separated list of atomic constraint identfiers.
  - `<propagated>` A single atomic constraint identifier.
  - `c:<constraint tag>`: _Optional_. A hint which constraint triggered the inference.
  - `l:<filtering algorithm>`: _Optional_. A hint which filtering algorithm identified the inference.

### Nogood
A nogood step encodes a partial assignment which cannot be extended to a solution.
```
n <step_id> <atomic constraint ids> [0 <propagation hint>]
```

The individual components:
  - `<step_id>`: A non-zero integer which serves as a unique identifier for the step in the proof.
  - `<atomic constraint ids>` A space-separated list of atomic constraint identfiers. These encode the nogood _as a clause_.
  - `0 <propagation hint>`: _Optional_. A hint which is a separated list of step ids, noting what order the steps can be applied to derive this nogood.

### Deletion
A deletion step can be used to indicate a nogood will no-longer be used in the derivation of new nogoods.
```
d <step_id>
```

### Conclusion
The conclusion finishes the proof. It is either the claim the problem is unsatisfiable:
```
c UNSAT
```

Or it is the claim of a dual bound for the objective variable:
```
c <objective bound>
```
where `<objective bound>` is an atomic constraint id encoding the dual bound on the objective variable.
