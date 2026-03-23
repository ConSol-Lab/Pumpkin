
# Constructor

The constructor encapsulates the information required to create an instance of the propagator. In our case, it stores:

- the variables `a` and `b` involved in the equality constraint, and
- a `ConstraintTag`, which Pumpkin uses internally to label inference steps for proof logging and explanation generation.

Here is the definition, containing only the data held by the constructor’s [`struct`](https://doc.rust-lang.org/book/ch05-00-structs.html):

```rust,no_run,noplayground
#[derive(Clone, Debug)]
pub struct BinaryEqualsPropagatorConstructor<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
    pub constraint_tag: ConstraintTag,
}
```

> **Notes**
> - The type parameters `AVar` and `BVar` are [generic](https://doc.rust-lang.org/book/ch10-01-syntax.html), meaning the propagator can work with any integer variable type Pumpkin supports. Rust generics play a role similar to Java generics and C++ templates, although they behave differently in important ways.
> - The fields are declared as [`pub`](https://doc.rust-lang.org/book/ch07-03-paths-for-referring-to-an-item-in-the-module-tree.html#exposing-paths-with-the-pub-keyword), allowing other [crates](https://doc.rust-lang.org/book/ch07-01-packages-and-crates.html) to construct this propagator.
> - The [derived traits](https://doc.rust-lang.org/book/appendix-03-derivable-traits.html#appendix-c-derivable-traits) `Clone` and `Debug` are added for convenience. Traits are conceptually similar to Java interfaces or C++ abstract base classes, but not identical.

Before implementing the constructor, we first define the *propagator struct* so the constructor has a concrete type to reference:

```rust,no_run,noplayground
#[derive(Clone, Debug)]
pub struct BinaryEqualsPropagator<AVar, BVar> {
    a: AVar,
    b: BVar,
    inference_code: InferenceCode,
}
```

This struct represents the propagator. It stores the two variables involved in the constraint, as well as an `inference_code`, whose purpose will become clearer later.

To complete the constructor, we implement the `PropagatorConstructor` [trait](https://doc.rust-lang.org/book/ch10-02-traits.html). A trait behaves like an interface: it specifies which methods must be provided. Here, it requires us to implement the `create` function, which tells Pumpkin how to:

1. register the propagator with the solver, and
2. construct the propagator instance.

The function signature is:

```rust,no_run,noplayground
fn create(
    self,
    mut context: PropagatorConstructorContext,
) -> Self::PropagatorImpl
```

The `context` is Pumpkin’s handle used during construction. Within this method, we register each variable with the solver:

```rust,no_run,noplayground
context.register(
    self.a.clone(),
    DomainEvents::ANY_INT,
    LocalId::from(0),
);

context.register(
    self.b.clone(),
    DomainEvents::ANY_INT,
    LocalId::from(1),
);
```

Let us examine each part of a `register` call:

- `self.a.clone()` — provides the variable to register. The clone is used for simplicity.
- `DomainEvents::ANY_INT` — indicates the propagator should be notified about *any* change to the variable’s domain, including bounds updates and value removals. Since this propagator maintains domain consistency, it must react to all such events. Other propagators may listen to other domain events, see the documentation for `DomainEvent`.
- `LocalId::from(0)` — assigns a numeric ID for how Pumpkin identifies this variable *within this propagator*. When `a` changes, Pumpkin will report that “local variable 0” changed. This indirection simplifies internal bookkeeping.

We perform a similar registration for `b`, using ID `1`, and then construct and return the actual propagator.

Below is the full implementation of the propagator constructor:

```rust,no_run,noplayground
impl<AVar, BVar> PropagatorConstructor
    for BinaryEqualsPropagatorConstructor<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryEqualsPropagator<AVar, BVar>;

    fn create(
        self,
        mut context: PropagatorConstructorContext,
    ) -> Self::PropagatorImpl {
        context.register(
            self.a.clone(),
            DomainEvents::ANY_INT,
            LocalId::from(0),
        );

        context.register(
            self.b.clone(),
            DomainEvents::ANY_INT,
            LocalId::from(1),
        );

        BinaryEqualsPropagator {
            a: self.a,
            b: self.b,
            inference_code: InferenceCode::new(self.constraint_tag, BinaryEquals),
        }
    }
}
```

> **Notes**
> - The constructor works with any integer variable type supported by Pumpkin that implements the `IntegerVariable` trait.
> - `PropagatorImpl` declares that this constructor produces `BinaryEqualsPropagator` instances.
> - The constructor builds and returns the propagator, along with its associated `InferenceCode`.

Once this constructor is in place, Pumpkin knows both **when** to invoke the propagator and **how** to initialise it. The next step is to implement the propagation logic itself.

### TODO
- Explain who actually creates the constructor. 
- The constructor has a "inference_code", but here we did not show how this value is set. 
- It might be confusing for people, since `create` sounds like it constructs the PropagatorConstructor, but instead it actually creates the propagator and not the constructor.