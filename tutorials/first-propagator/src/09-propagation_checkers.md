# Propagation Checkers

## TODO
- I am still processing this section.

Propagation checkers are independent procedures that verify whether a given explanation is correct. Because Pumpkin requires every propagator to provide explanations for all propagations and conflicts, checkers allow us to validate that these explanations are justified.

Since checkers are implemented separately from the propagator with much simpler correctness‑focused logic rather than performance‑optimised code, they serve as a reliable reference implementation. This separation increases our confidence that the behavaiour of the propagator is correct.

There are two parts when checking explanation.
- Applicability: asssert that the explanation is relevant given the trail, that is, that the premise of the explanation is satisfied, and that this is what triggered the propagation. For example, if the propagator claims it removed the value 5 from the domain of variable `b` because the variable `a` does not have 5 in its domain ((\\[a != 5] -> [b != 5]\\)), we need to check that indeed $a != 5$ holds. Since this is generic over all propagators, Pumpkin does this behind the scenes and we do not need to worry about this.
1. **Applicability**: assert that the explanation is relevant given the trail. In other words, the premises of the explanation are satisfied and that the consequent was not set before the premise. For example, if the propagator claims it removed the value 5 from the domain of variable `b` because variable `a` does not contain 5 (i.e., `[a != 5] -> [b != 5]`), we must check that indeed `a != 5` holds and that `b != 5` has not been set before. Since this check is generic across propagators, Pumpkin performs this behind the scenes and we do not need to explicitly consider it.
2. **Validity**: assert that the explanation is logically entailed by the constraint. This is what each propagator must verify, and what we focus on below.

## Inference Checker Trait

Inference checkers verify the validity of an explanation. These checkers have their own trait, the `InferenceChecker` trait, which defines a single method `check`:

```rust
pub trait InferenceChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    fn check(
        &self,
        state: VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool;
}
```

Given an explanation of the form `∧ premises → consequent`, the `VariableState` contains domains such that all premises hold and (if present) the **negation** of the consequent holds. It serves a similar role to the `PropagationContext` used during propagation, acting as the object that provides information about current variable domains. Note that the consequent is `None` when checking explanations for conflicts.

The checker is expected to return `true` if the `state` leads to a conflict, indicating that the checker successfully verified the correctness of the explanation. Conceptually, verifying an explanation reduces to only checking for conflicts, since propagation explanations are converted into conflict explanations. As we only use checkers during testing, our checker logic can be much simpler than the propagator. 

## Checker Implementation

For the `a = b` constraint, we know that a conflict exists when the domains of `a` and `b` have an empty intersection. In other words, if there is no value that both variables can take, the constraint is violated.

The checker implements this idea as follows:

1. Enforce `a <= UB(b)` and `a >= LB(b)`.
2. Remove from `a` all values that are not present in `b`.

If applying these restrictions results in an empty domain under the domain encoded in `state`, then the explanation is valid.

We begin by defining a `struct` that identifies the variables involved in the explanation:

```rust
#[derive(Clone, Debug)]
pub struct BinaryEqualsChecker<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
}
```

STOPPED HERE. Need to explain IntExt

```rust
impl<AVar, BVar, Atomic> InferenceChecker<Atomic> for BinaryEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    AVar: CheckerVariable<Atomic>,
    BVar: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        mut state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        let mut consistent = true;

        // Process upper bound of b
        if let IntExt::Int(value) = self.b.induced_upper_bound(&state) {
            let atomic = self.a.atomic_less_than(value);
            consistent &= state.apply(&atomic);
        }

        // Process lower bound of b
        if let IntExt::Int(value) = self.b.induced_lower_bound(&state) {
            let atomic = self.a.atomic_greater_than(value);
            consistent &= state.apply(&atomic);
        }

        // Remove values absent from b (holes in b)
        for value in self.b.induced_holes(&state).collect::<Vec<_>>() {
            let atomic = self.a.atomic_not_equal(value);
            consistent &= state.apply(&atomic);
        }

        !consistent
    }
}
```

### Step-by-step notes

**Process the upper bound.** We begin by retrieving the upper bound of `b` using `self.b.induced_upper_bound`. The pattern-matching statement

```rust
let IntExt::Int(b_ub) = self.b.induced_upper_bound(&state)
```

checks whether the returned upper bound value is an `IntExt::Int`, which in our setting indicates that the upper bound is a finite integer. Note that the value could be unbounded if the explanation did not specify bounds, in which case the value would be `IntExt::NegativeInf` or `PositiveInt`. If the match succeeds, the variable `b_ub` holds the finite upper bound of `self.b` and the subsequent code block is executed; otherwise, the pattern does not match, and the block is skipped. Once we know that the upper bound of `b` is constrained in the state, we create a constraint stating that `a <= b_ub` and apply it to the state, keeping track in the variable `consistent` whether or not the state remains consistent.

**Process the lower bound.** We apply the same reasoning as for the upper bound, but now for the lower bound of `b`.

**Process holes.** To ensure that `a` does not contain values absent from the domain of `b`, we retrieve the *holes* of `b`—the values between its bounds that are **not** in its domain—using `self.b.induced_holes`. We then iterate over these values and apply the corresponding removal constraints, in the same manner as for the bound-based checks.

**Return statement.** The checker considers the inference correct if these derived constraints lead to an inconsistency in the checking state; otherwise, under this local reasoning, the explanation does not justify the inference.

## A Simple Test for the Checker

Finally, we recommend validating the checker with a unit test (see the Rust Book, Ch. 11). The following test constructs a state in which the premises imply `b <= 10` and tests whether the inference `a <= 10` is accepted by the checker.

```rust
#[cfg(test)]
mod tests {
    use super::*;
    // ... imports elided

    #[test]
    fn checker_accepts_a_leq_b_ub() {
        let premises = [TestAtomic {
            name: "b",
            comparison: pumpkin_checking::Comparison::LessEqual,
            value: 10,
        }];
        let consequent = Some(TestAtomic {
            name: "a",
            comparison: pumpkin_checking::Comparison::LessEqual,
            value: 10,
        });

        let state = VariableState::prepare_for_conflict_check(&premises, consequent.as_ref())
            .expect("premises are consistent");

        let checker = BinaryEqualsChecker { a: "a", b: "b" };

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }
}
```

**Notes**
- `#[cfg(test)]` and the `tests` module ensure this code only compiles when you run `cargo test`.
- Each test function is annotated with `#[test]`.
- `VariableState::prepare_for_conflict_check` may return an error if given inconsistent atomic constraints (e.g., `[a = 1] ∧ [a ≠ 1] → ⊥`).

If this test passes, the checker can identify when the intersection of the domains of `a` and `b` is empty. In the next step, you can test the full propagator with `TestSolver` to further increase confidence in correctness.