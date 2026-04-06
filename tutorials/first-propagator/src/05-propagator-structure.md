# Propagator Structure

Now that we have seen how the propagators should behave, we take a step back and examine how to represent this propagator within Pumpkin.

A propagator in Pumpkin is built from three core components:

1. **A constructor** – specifies which variables the propagator operates on and registers the domain events it should listen to.
2. **The propagator implementation** – contains the propagation logic executed by the solver.
3. **A consistency checker** – verifies that every value removed from or retained in a domain is properly justified, ensuring that the propagator behaves correctly.

These components allow Pumpkin to create, schedule, and execute propagators efficiently during search, while also providing an automated mechanism for verifying their correctness.