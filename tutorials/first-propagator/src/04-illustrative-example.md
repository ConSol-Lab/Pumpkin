# Illustrative Example

To understand how the equality constraint

$$ a = b $$

behaves during propagation, let us walk through a concrete example. The propagator ensures that both variables eventually take the **same value**, meaning their domains must shrink to the **intersection** of the original domains.

Consider the following initial domains:

$$ a \in [5, 8] $$
$$ b \in \\{4, 5, 7, 9\\} $$

Propagation proceeds in two phases.

### Filtering the domain of `a`

- _Bounds filtering_: The bounds of `b` (4 and 9) do not constrain the bounds of `a`.
- _Value filtering_: The values 6 and 8 are not present in the domain of `b`, so they can be removed from `a`.

After filtering:

$$ a = \\{5, 7\\} $$

### Filtering the domain of `b`

- _Bounds filtering_: The bounds of `a` (5 and 7) constrain the bounds of `b`, removing 4 and 9.
– _Value filtering_: Both 5 and 7 appear in the domain of `a`, and 6 does not, so no additional values need to be removed from `b`.

After filtering:

$$ b = \\{5, 7\\} $$

### Final State

Both variables have the same domain:

$$ a = \\{5, 7\\}, \qquad b = \\{5, 7\\} $$

This is exactly the behaviour our propagator must reproduce.