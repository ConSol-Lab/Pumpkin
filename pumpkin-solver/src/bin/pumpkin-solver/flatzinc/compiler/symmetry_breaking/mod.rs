//! Detect interchangeable variables and post lex-leader constraints to break the symmetry.
//!
//! The pass runs after the constraints are posted, so every variable is resolvable, and reads
//! the constraints from the AST. It has three stages:
//!
//! 1. [`canonical_form`] reduces the model to a variable/constraint graph in which two variable
//!    occurrences share a slot key exactly when exchanging them cannot change the constraint.
//! 2. [`colour_refinement`] and [`candidate_permutations`] propose permutations of variables that
//!    the graph structure cannot distinguish.
//! 3. [`automorphism_check`] verifies each proposal syntactically against the whole model;
//!    [`lex_leader`] posts a lex-leader constraint for each verified symmetry.
//!
//! For example, take `x`, `y` and `z` in `0..3` with the constraints
//!
//! ```text
//! x + y <= 4,   x != z,   y != z
//! ```
//!
//! Exchanging `x` and `y` maps the first constraint to itself and the other two onto each other,
//! so it is a symmetry. The pass posts `x <= y`, which keeps the solution `x = 1, y = 3, z = 0`
//! and removes its image `x = 3, y = 1, z = 0`.
//!
//! Only stage 3 decides what is posted, so an over-eager proposal costs time but not soundness.
//! Symmetry breaking removes solutions, so it is refused when all solutions of a satisfaction
//! problem are requested. It is also refused under proof logging, because the posted constraints
//! are not implied by the model and the proof format has no step to justify them yet.

mod automorphism_check;
mod candidate_permutations;
pub(super) mod canonical_form;
mod colour_refinement;
mod lex_leader;
#[cfg(test)]
mod tests;

use std::time::Duration;
use std::time::Instant;

use flatzinc::Goal;
use log::info;

use super::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;
use crate::flatzinc::ast::FlatZincAst;

/// Wall-clock limit on proposing candidates. Verification and posting are not limited; they are
/// linear in the model.
const DETECTION_TIME_LIMIT: Duration = Duration::from_secs(30);

/// How many colour classes are tried as seeds for candidate permutations.
const MAX_SEED_CLASSES: usize = 8;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if !options.symmetry_breaking {
        return Ok(());
    }
    if options.proof_type.is_some() {
        return Err(FlatZincError::SymmetryBreakingUnsupported("proof logging"));
    }
    if options.all_solutions && matches!(ast.solve_item.goal, Goal::Satisfy) {
        return Err(FlatZincError::SymmetryBreakingUnsupported(
            "enumerating all solutions of a satisfaction problem",
        ));
    }

    let started = Instant::now();

    let index = canonical_form::ModelIndex::build(ast, context)?;
    let graph = index.canonicalise(ast)?;

    let colouring = colour_refinement::refine(
        &graph,
        colour_refinement::initial_colouring(&index.variables, index.objective),
    );
    // A variable that occurs in no constraint is trivially interchangeable with every other
    // such variable; breaking that removes nothing worth removing.
    let mut classes = colour_refinement::colour_classes(&colouring);
    for class in &mut classes {
        class.retain(|&var| !graph.incident[var].is_empty());
    }
    classes.retain(|class| class.len() > 1);
    if classes.is_empty() {
        info!(
            "symmetry breaking: no interchangeable variables ({} variables, {} constraints, {:.2}s)",
            graph.num_variables(),
            graph.constraints.len(),
            started.elapsed().as_secs_f64()
        );
        return Ok(());
    }

    let mut class_sizes: Vec<usize> = classes.iter().map(Vec::len).collect();
    class_sizes.sort_unstable();
    class_sizes.dedup();
    info!(
        "symmetry breaking: {} candidate classes with sizes {:?}",
        classes.len(),
        class_sizes
    );

    let budget = candidate_permutations::Budget::new(DETECTION_TIME_LIMIT, MAX_SEED_CLASSES);
    let candidates = candidate_permutations::candidate_swaps(&graph, &colouring, &classes, &budget);

    let mut posted = 0;
    let mut rejected = 0;
    for permutation in &candidates {
        if automorphism_check::is_automorphism(
            &graph,
            &index.variables,
            index.objective,
            permutation,
        ) {
            lex_leader::post(context.solver, &index, permutation);
            posted += 1;
            log::debug!(
                "symmetry breaking: broke a symmetry moving {} variables",
                permutation.support().len()
            );
        } else {
            rejected += 1;
        }
    }

    info!(
        "symmetry breaking: {} candidate classes, {} candidates, {} symmetries broken, {} rejected ({:.2}s)",
        classes.len(),
        candidates.len(),
        posted,
        rejected,
        started.elapsed().as_secs_f64()
    );

    Ok(())
}
