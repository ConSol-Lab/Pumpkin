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
//! problem are requested. Under proof logging in the croc block format, [`proof`] writes a
//! lex-leader step for each lex-leader constraint of 0/1 variables whose constraints the checker
//! decides, and posts the constraint; under other proof logging,
//! including every DRCP proof, the pass is refused, because the posted constraints are not
//! implied by the model.

mod automorphism_check;
mod candidate_permutations;
pub(super) mod canonical_form;
mod colour_refinement;
mod lex_leader;
mod proof;
#[cfg(test)]
mod tests;
mod value_precedence;
mod value_symmetry;
mod value_symmetry_proof;

use std::time::Duration;
use std::time::Instant;

use flatzinc::Goal;
use log::info;

use super::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;
use crate::flatzinc::ast::FlatZincAst;

/// Wall-clock limit on refinement and proposing candidates, and separately on verifying them;
/// candidates not verified in time are skipped. Reading the model and posting are not limited.
/// On the challenge instances 5 s kept 8,275 of the 11,129 symmetries found with 30 s, and the
/// saved time outweighed the lost symmetries in the primal integral.
const DETECTION_TIME_LIMIT: Duration = Duration::from_secs(5);

/// How many colour classes are tried as seeds for candidate permutations, largest first.
const MAX_SEED_CLASSES: usize = 1000;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if !options.symmetry_breaking {
        return Ok(());
    }
    if !options.proof_states_breaking_steps() {
        return Err(FlatZincError::SymmetryBreakingUnsupported(
            "proof logging other than with --proof-format croc and the scaffold or block proof type",
        ));
    }
    if options.all_solutions && matches!(ast.solve_item.goal, Goal::Satisfy) {
        return Err(FlatZincError::SymmetryBreakingUnsupported(
            "enumerating all solutions of a satisfaction problem",
        ));
    }

    let started = Instant::now();

    // A model the pass cannot read, e.g. one with a constraint argument of an unexpected form,
    // is solved without symmetry breaking instead of being rejected.
    let (index, graph) = match canonical_form::ModelIndex::build(ast, context)
        .and_then(|index| index.canonicalise(ast).map(|graph| (index, graph)))
    {
        Ok(read) => read,
        Err(error) => {
            info!("symmetry breaking: skipped, the model could not be read: {error}");
            return Ok(());
        }
    };

    let read_time = started.elapsed();
    let deadline = Instant::now() + DETECTION_TIME_LIMIT;
    let converged = colour_refinement::refine(
        &graph,
        colour_refinement::initial_colouring(&index.variables, index.objective),
        deadline,
    );
    let colouring = converged.colouring();
    // A variable that occurs in no constraint is trivially interchangeable with every other
    // such variable; breaking that removes nothing worth removing.
    let mut classes = colour_refinement::colour_classes(&colouring);
    for class in &mut classes {
        class.retain(|&var| !graph.incident[var].is_empty());
    }
    classes.retain(|class| class.len() > 1);
    // Larger classes first: they carry the larger symmetry groups. Ties by smallest variable,
    // so the order depends on the model only.
    classes.sort_by(|a, b| b.len().cmp(&a.len()).then(a[0].cmp(&b[0])));
    let refine_time = started.elapsed() - read_time;
    if classes.is_empty() {
        info!(
            "symmetry breaking stages: read {:.2}s, refine {:.2}s",
            read_time.as_secs_f64(),
            refine_time.as_secs_f64()
        );
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

    let budget = candidate_permutations::Budget::until(deadline, MAX_SEED_CLASSES);
    let candidates = candidate_permutations::candidate_swaps(&graph, &converged, &classes, &budget);
    let propose_time = started.elapsed() - read_time - refine_time;
    let mut verify_time = Duration::ZERO;
    let mut post_time = Duration::ZERO;
    let store = automorphism_check::constraint_set(&graph);
    let verification_deadline = Instant::now() + DETECTION_TIME_LIMIT;

    // Under proof logging, the symmetries whose steps the checker decides are kept, and the
    // order and steps are written once all are known, since the order comes first.
    let mut symmetry_proof = context
        .solver
        .is_logging_proof()
        .then(|| proof::SymmetryProof::new(ast, &index, &graph, context));
    let mut posted = 0;
    let mut rejected = 0;
    for permutation in &candidates {
        if Instant::now() >= verification_deadline {
            rejected += 1;
            continue;
        }
        let verify_start = Instant::now();
        let is_symmetry = automorphism_check::is_automorphism(
            &graph,
            &store,
            &index.variables,
            index.objective,
            permutation,
        );
        verify_time += verify_start.elapsed();
        if is_symmetry {
            let post_start = Instant::now();
            match &mut symmetry_proof {
                Some(symmetry_proof) => {
                    if symmetry_proof.keep(ast, &index, &graph, permutation) {
                        posted += 1;
                    } else {
                        rejected += 1;
                    }
                }
                None => {
                    lex_leader::post(context.solver, &index, permutation);
                    posted += 1;
                }
            }
            post_time += post_start.elapsed();
            log::debug!(
                "symmetry breaking: broke a symmetry moving {} variables",
                permutation.support().len()
            );
        } else {
            rejected += 1;
        }
    }

    if let Some(symmetry_proof) = &symmetry_proof {
        symmetry_proof.log_and_post(context, &index);
        info!(
            "symmetry breaking: {} symmetries logged as lex-leader steps",
            symmetry_proof.logged()
        );
    }
    info!(
        "symmetry breaking: {} candidate classes, {} candidates, {} symmetries broken, {} rejected ({:.2}s)",
        classes.len(),
        candidates.len(),
        posted,
        rejected,
        started.elapsed().as_secs_f64()
    );
    info!(
        "symmetry breaking stages: read {:.2}s, refine {:.2}s, propose {:.2}s, verify {:.2}s, post {:.2}s",
        read_time.as_secs_f64(),
        refine_time.as_secs_f64(),
        propose_time.as_secs_f64(),
        verify_time.as_secs_f64(),
        post_time.as_secs_f64()
    );

    Ok(())
}

/// Detect interchangeable values and post value-precedence constraints; see [`value_symmetry`].
pub(crate) fn run_value_symmetry(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if !options.value_symmetry_breaking {
        return Ok(());
    }
    let unsupported = FlatZincError::ValueSymmetryBreakingUnsupported;
    if !options.proof_states_breaking_steps() {
        return Err(unsupported(
            "proof logging other than with --proof-format croc and the scaffold or block proof type",
        ));
    }
    if options.all_solutions && matches!(ast.solve_item.goal, Goal::Satisfy) {
        return Err(unsupported(
            "enumerating all solutions of a satisfaction problem",
        ));
    }
    if options.symmetry_breaking || options.dominance_breaking || options.dual_fixing {
        return Err(unsupported("other symmetry or dominance breaking"));
    }

    let started = Instant::now();
    // A model the pass cannot read, e.g. one with a constraint argument of an unexpected form,
    // is solved without value symmetry breaking instead of being rejected.
    let (index, graph) = match canonical_form::ModelIndex::build(ast, context)
        .and_then(|index| index.canonicalise(ast).map(|graph| (index, graph)))
    {
        Ok(read) => read,
        Err(error) => {
            info!("value symmetry breaking: skipped, the model could not be read: {error}");
            return Ok(());
        }
    };
    let found = value_symmetry::interchangeable_values(
        &graph,
        &index.variables,
        index.objective,
        started + DETECTION_TIME_LIMIT,
    );
    if context.solver.is_logging_proof() {
        // Only the exchanges the checker decides are stated, and the constraints are not posted,
        // since no rule of the checker licenses an inference from them.
        let logged: Vec<&value_symmetry::InterchangeableValues> = found
            .iter()
            .filter(|interchangeable| {
                value_symmetry_proof::is_logged(&graph, &index, interchangeable)
            })
            .collect();
        value_symmetry_proof::log_steps(context, &index, &logged);
        info!(
            "value symmetry breaking: {} of {} groups logged as value symmetry steps, {} steps",
            logged.len(),
            found.len(),
            logged
                .iter()
                .map(|interchangeable| interchangeable.values.len() - 1)
                .sum::<usize>()
        );
    } else {
        for interchangeable in &found {
            value_precedence::post(context.solver, &index, interchangeable);
        }
    }
    info!(
        "value symmetry breaking: {} groups with interchangeable values, sizes {:?}, values {:?} ({:.2}s)",
        found.len(),
        found.iter().map(|f| f.variables.len()).collect::<Vec<_>>(),
        found.iter().map(|f| f.values.len()).collect::<Vec<_>>(),
        started.elapsed().as_secs_f64()
    );
    Ok(())
}
