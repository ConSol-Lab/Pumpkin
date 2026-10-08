# Propagation Logic

With the constructor in place, we can now implement the propagation logic by implementing the `Propagator` trait. This trait defines several methods, but the core function we must implement is:

```rust,no_run,noplayground
fn propagate_from_scratch(
	&self, 
	mut context: PropagationContext
) -> Result<(), Conflict>
```

The `PropagationContext` gives access to the current domains of all variables and provides methods for posting domain updates.

This section walks through the implementation step by step.

## Reading Domains

We begin by retrieving the current bounds of both variables:

```rust,no_run,noplayground
let a_lb = context.lower_bound(&self.a);
let a_ub = context.upper_bound(&self.a);
let b_lb = context.lower_bound(&self.b);
let b_ub = context.upper_bound(&self.b);
```

> **Note**
> - lb (or LB) and ub (or UB) are common abbreviations for the lower and upper bound of a variable. These are standard terms, so we recommend getting confortable with them.

## Changing Domains

Propagation modifies the domains of variables, so let us look at how to perform domain changes in Pumpkin. Suppose we want to tell Pumpkin that the lower bound of variable `a` must be at least the lower bound of `b`: \\(a \ge LB(b)\\).

Domain modifications are done by asserting that an *atomic constraint* must hold. An atomic constraint is an elementary constraint consisting of a single variable, a comparator (\<=, \>=, ==, !=), and an integer (`i32`).

In Pumpkin, atomic constraints are represented by the `Predicate` struct. They are typically constructed using macros for convenience.

The first macro you will learn is the `predicate!` macro. Here are four examples:

```rust,no_run,noplayground
// [a >= b_lb]
let p1 = predicate!(self.a >= b_lb);
// [a <= b_ub]
let p2 = predicate!(self.a <= b_ub);
// [a != b_lb]
let p3 = predicate!(self.a != b_lb);
// [a == b_ub]
let p4 = predicate!(self.a == b_ub);
```

To request a domain update in Pumpkin, we use `context.post`:

```rust,no_run,noplayground
context.post(
    predicate!(self.a >= b_lb),
    conjunction!([self.b >= b_lb]),
    &self.inference_code,
)?;
```

Let us unpack what happens here. The `context.post` function takes three arguments:

- `predicate!(self.a >= b_lb)`: The atomic constraint that must be enforced.
- We use the `conjunction!` macro (https://doc.rust-lang.org/book/ch20-05-macros.html#macros), which constructs a `PropositionalConjunction`. This macro is used frequently, so it is worth becoming comfortable with it. The conjunction represents a set of atomic constraints that describe the *reason* why the propagation occurred. In this example, we state that `b >= b_lb` _caused_ the propagation `a >= b_lb`. The reason may involve any number of atomic constraints, as we will see shortly.
- `&self.inference_code`: This identifies the inference mechanism used by the propagator. Our `a = b` propagator uses only a single inference type, but in general a propagator may have multiple inference mechanisms. Internally, the inference code is used internally to verify that a domain change or conflict is justified given the explanation and the inference rule. This will become clearer once we discuss checkers and testing.

The trailing `?` is essential. Asking Pumpkin to update a domain may fail if the result is an empty domain (a conflict). For example, posting `a >= 5` when the current domain of `a` is `[0, 3]` produces an empty domain. The `?` operator immediately propagates such a failure upward. Internally, `context.post` returns `Result<(), EmptyDomainConflict>`. Always use `?`; never construct an `EmptyDomainConflict` manually.

We are now ready to proceed to the propagation algorithm.

## Conflict Check

If both variables are already assigned to different values, the constraint `a = b` is violated. In general, it is common to check for conflicts before performing propagation, because an early check can often simplify the propagation algorithm that follows.

```rust,no_run,noplayground
if a_ub < b_lb {
    return propagator_conflict(
        conjunction!([self.a <= a_ub] & [self.b >= b_lb]),
        &self.inference_code,
    );
}

if a_lb > b_ub {
    return propagator_conflict(
        conjunction!([self.a >= a_lb] & [self.b <= b_ub]),
        &self.inference_code,
    );
}
```

Let us unpack the snippet above: When a conflict is detected, the propagator signals it by returning an error. The error consists of the conjunction of predicates that cause the conflict, as well as the inference code that identified the conflict. In the `conjunction!` macro, predicates are joined using the `&` operator to represent `logical and`.

## Bounds Filtering

We ensure the bounds of `a` and `b` are identical using the following logic:

$$ LB(a) \ge LB(b) $$
$$ UB(a) \le UB(b) $$
$$ LB(b) \ge LB(a) $$
$$ UB(b) \le UB(a) $$

This is implemented by calling `context.post(...)`, which may tighten bounds or detect an empty domain.

```rust,no_run,noplayground
context.post(
    predicate!(self.a >= b_lb),
    conjunction!([self.b >= b_lb]),
    &self.inference_code,
)?;
context.post(
    predicate!(self.a <= b_ub),
    conjunction!([self.b <= b_ub]),
    &self.inference_code,
)?;
context.post(
    predicate!(self.b >= a_lb),
    conjunction!([self.a >= b_lb]),
    &self.inference_code,
)?;
context.post(
    predicate!(self.b <= a_ub),
    conjunction!([self.a <= a_ub]),
    &self.inference_code,
)?;
```
> **Note:**
> Every call to `post` has the trailing `?` we discussed earlier. It ensures the propagator reports a conflict if applying the domain change results in an empty domain.

## Value Filtering

Lastly, we remove values that appear in one domain but not the other. The helper
method `get_holes` returns all values between the bounds that are currently *absent* from a variable’s domain:

```rust,no_run,noplayground
for removed_value_a in context.get_holes(&self.a).collect::<Vec<_>>() {
    context.post(
        predicate!(self.b != removed_value_a),
        conjunction!([self.a != removed_value_a]),
        &self.inference_code,
    )?;
}

for removed_value_b in context.get_holes(&self.b).collect::<Vec<_>>() {
    context.post(
        predicate!(self.a != removed_value_b),
        conjunction!([self.b != removed_value_b]),
        &self.inference_code,
    )?;
}
```
> **Note:**
> We call `collect` to evaluate `get_holes` up front and store the result in a Vec. This avoids borrow‑checker issues, because Rust does not allow iterating over the vector of holes while also calling `post`, which may mutate that same vector. By collecting first, we iterate over the current snapshot of the holes. This approach works for now, though we may revise it in the future.

This completes the domain-consistent filtering algorithm for our `a = b` propagator.

## Full Propagator Implementation

```rust,no_run,noplayground
impl<AVar: IntegerVariable + 'static, BVar: IntegerVariable + 'static>
    Propagator for BinaryEqualsPropagator<AVar, BVar>
{
    fn propagate_from_scratch(&self, mut context: PropagationContext) -> Result<(), Conflict> {
        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);
        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        if a_lb == a_ub && b_lb == b_ub && a_lb != b_lb {
            return Err(Conflict::Propagator(PropagatorConflict {
                conjunction: conjunction!([self.a == a_lb] & [self.b == b_lb]),
                inference_code: self.inference_code.clone(),
            }));
        }

        context.post(
            predicate!(self.a >= b_lb),
            conjunction!([self.b >= b_lb]),
            &self.inference_code,
        )?;
        context.post(
            predicate!(self.a <= b_ub),
            conjunction!([self.b <= b_ub]),
            &self.inference_code,
        )?;
        context.post(
            predicate!(self.b >= a_lb),
            conjunction!([self.a >= b_lb]),
            &self.inference_code,
        )?;
        context.post(
            predicate!(self.b <= a_ub),
            conjunction!([self.a <= a_ub]),
            &self.inference_code,
        )?;

        for removed_value_a in context.get_holes(&self.a).collect::<Vec<_>>() {
            context.post(
                predicate!(self.b != removed_value_a),
                conjunction!([self.a != removed_value_a]),
                &self.inference_code,
            )?;
        }

        for removed_value_b in context.get_holes(&self.b).collect::<Vec<_>>() {
            context.post(
                predicate!(self.a != removed_value_b),
                conjunction!([self.b != removed_value_b]),
                &self.inference_code,
            )?;
        }

        Ok(())
    }
}
```

### TODO
- use the `fixed_value` function and modify this page accordingly.
- could reference the conjunction macro documentation
- this is a bit ugly: `Err(Conflict::Propagator(PropagatorConflict {...}))`. Maybe we could something to make this nicer.
