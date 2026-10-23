# Consistency Checkers

Propagators can easily contain subtle bugs, so Pumpkin includes **checkers** that verify whether a propagator behaves correctly during runtime.

We now focus on **consistency checkers**, which certify that a propagator is consistent at a fixpoint. Pumpkin uses two types of checkers:

- **Propagation checkers** — verify that value removals and conflicts are correctly justified.
- **Retention checkers** — verify that every value *kept* in a domain is justified.

Both rely on certificates: small objects that can be checked independently to ensure that the required properties are satisfied. Because certificate checking is simple and reliable, it gives us strong assurance that the propagator behaves correctly.

These certificates are checked at runtime while solving. Because this incurs overhead, checkers are typically enabled only during testing.

In the remainder of this section, we implement checkers for our `a = b` propagator.

> **Note:**
> In a typical development workflow, we recommend implementing the checker *before* implementing the propagator logic. This makes it easier to validate correctness while developing the propagation logic.
> For this tutorial, we reversed the order so you can first see how the propagator works in practice.