# Propagation Checkers

## TODO
- I did not make a detailed pass through this section.

## Checker Struct

We define a `struct` that identifies which variables are involved in the inference.

```rust
#[derive(Clone, Debug)]
pub struct BinaryEqualsChecker<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
}
```

---

## Checker Trait
As with other structs, the checker has its own trait, the `InferenceChecker` trait, which defines a single method `check`:

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

Given an explanation for a propagation or conflict (\\(\bigwedge_{p \in \mathtt{premises}} p \rightarrow \mathtt{consequent}\\)), the `VariableState` represents variable domains such that all premises hold and (if present) the **negation** of the consequent holds. It serves a role similar to the `PropagationContext` used during propagation, acting as the main object that provides information about the variables. Note that the consequent can be `None` if the explanation was for a conflict rather than a propagation.

The checker returns `true` if the `state` leads to a conflict, indicating that the checker successfully verified the correctness of the explanation. Since verifying the explanation corresponds to detecting a conflict, our checker logic can be much simpler than the propagator.

We know that for a conflict to exist, the intersection of the domains must be empty, since otherwise the variables could be assigned to the same value. We implement this by applying the domain of `b` to the domain of `a` and checking whether the result is an empty domain.

## Checker Implementation

The following implementation of the checker for our `a = b` propagator follows the above idea:

1. Enforce \(a \leq \mathrm{UB}(b)\) and \(a \geq \mathrm{LB}(b)\).
2. Remove from `a` all values not present in `b`.

If these steps lead to inconsistency under the premises and the negated consequent, the inference is correct.

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