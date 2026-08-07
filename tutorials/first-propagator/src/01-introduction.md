# Introduction

The goal of this tutorial is to help you become comfortable with the fundamental development workflow in **Pumpkin**, our constraint‑programming solver. We will guide you through the implementation, testing, and integration of a simple propagator, allowing you to gain hands‑on experience with the core ideas behind implementing propagators.

Pumpkin is implemented in [Rust](https://rust-lang.org), a modern systems programming language designed with a strong focus on **safety** and **performance**. If you are new to Rust, we recommend briefly reviewing the [Rust Book](https://doc.rust-lang.org/book/), particularly the sections on *ownership* and *borrowing*. You are not expected to master these topics for this tutorial—however, a light familiarity will make the examples easier to follow. When relevant, we include links to the corresponding Rust documentation.

This tutorial assumes basic familiarity with `git` and introductory concepts from constraint programming (specifically, Chapters 1 and 2 of the course lecture notes). By the end of this tutorial, you will know how to:

- Implement a propagator in Pumpkin
- Register and integrate it into the solving pipeline
- Write systematic tests using `TestSolver`
- Validate propagations using a checker
- Understand common pitfalls and how to avoid them
- Run Pumpkin on FlatZinc models generated via MiniZinc

We hope this tutorial helps you build intuition for how constraint propagation works under the hood and gives you the confidence to extend Pumpkin with new propagators.