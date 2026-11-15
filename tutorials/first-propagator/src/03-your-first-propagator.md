# Your First Propagator: `a = b`

We will go through the complete implementation of a toy propagator for the equality constraint

$$ a = b, $$

where _a_ and _b_ are integer variables.

This example introduces all core components of Pumpkin’s propagation framework: domain events, propagator constructors, propagation logic, explanation generation, and mechanisms to ensure the implementation is correct, namely propagation and retention checkers. By the end, you will have a clear understanding of how a simple constraint is translated into a fully functioning propagator.

Our implementation of the the propagator will enforce **domain consistency**, so only values that could be part of a feasible assignment remain in the domains of the variables. To achieve this, the propagator performs two types of filtering:

1. _Bounds Filtering_: Align the lower and upper bounds of both variables.

2. _Value Filtering_: Remove values that occur in the domain of one variable but not the other.

Before we examine the Rust implementation, let us first build intuition through a small example.