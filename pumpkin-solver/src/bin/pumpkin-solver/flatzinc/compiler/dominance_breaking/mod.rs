//! Detect dominated variables in linear models and post dominance-breaking constraints.
//!
//! Variable `x_j` dominates `x_k` when exchanging their values, in any solution with
//! `x_k > x_j`, gives a solution that is at least as good. For variables that occur only in
//! linear constraints this can be decided from the coefficients; see [`dominance_order`] for the
//! conditions and the soundness argument. For each dominated pair the pass posts `x_j >= x_k`;
//! the witness is the transposition of the two variables.
//!
//! For example, in the knapsack
//!
//! ```text
//! maximise 5a + 5b + 4c   subject to   2a + 3b + 3c <= 5,   a, b, c in {0, 1}
//! ```
//!
//! `a` dominates `b` (same profit, smaller weight) and `b` dominates `c` (larger profit, same
//! weight), so the pass posts `a >= b` and `b >= c`. This removes the solution `a = 0, b = 1,
//! c = 0`; exchanging `a` and `b` gives `a = 1, b = 0, c = 0`, which has the same profit and a
//! smaller weight.
//!
//! [`linear_model`] extracts the eligible variables and their coefficients from the canonical
//! form built for symmetry breaking.
//!
//! The pass is refused under proof logging, because the posted constraints are not implied by
//! the model; when all solutions of a satisfaction problem are requested, because it removes
//! solutions; and together with symmetry breaking, because the two passes order tied variables
//! in opposite directions and their joint soundness is not established.

mod dominance_order;
mod linear_model;
#[cfg(test)]
mod tests;

use std::time::Instant;

use flatzinc::Goal;
use log::info;
use pumpkin_solver::core::constraints::Constraint;
use pumpkin_solver::core::variables::Literal;

use super::context::CompilationContext;
use super::symmetry_breaking::canonical_form::ModelIndex;
use super::symmetry_breaking::canonical_form::VarKind;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;
use crate::flatzinc::ast::FlatZincAst;

/// Candidates with the same domain are compared pairwise; larger groups are skipped.
const MAX_GROUP: usize = 2000;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if !options.dominance_breaking {
        return Ok(());
    }
    if options.proof_type.is_some() {
        return Err(FlatZincError::DominanceBreakingUnsupported("proof logging"));
    }
    if options.symmetry_breaking {
        return Err(FlatZincError::DominanceBreakingUnsupported(
            "symmetry breaking",
        ));
    }
    if options.all_solutions && matches!(ast.solve_item.goal, Goal::Satisfy) {
        return Err(FlatZincError::DominanceBreakingUnsupported(
            "enumerating all solutions of a satisfaction problem",
        ));
    }

    let started = Instant::now();
    let index = ModelIndex::build(ast, context)?;
    let graph = index.canonicalise(ast)?;
    let model = linear_model::LinearModel::extract(&graph, &index, &ast.solve_item.goal);
    let (pairs, skipped) = dominance_order::pairs_to_post(&model, &index.variables, MAX_GROUP);

    for &(better, worse) in &pairs {
        let better_id = index.domain_ids[better];
        let worse_id = index.domain_ids[worse];
        let tag = context.solver.new_constraint_tag();
        match index.variables[better].kind {
            VarKind::Bool => pumpkin_constraints::binary_less_than_or_equals(
                Literal::new(worse_id),
                Literal::new(better_id),
                tag,
            )
            .post(context.solver),
            VarKind::Int => {
                pumpkin_constraints::binary_less_than_or_equals(worse_id, better_id, tag)
                    .post(context.solver)
            }
        }
    }

    info!(
        "dominance breaking: {} candidates, {} dominance constraints posted, {} candidates skipped ({:.2}s)",
        model.candidates.len(),
        pairs.len(),
        skipped,
        started.elapsed().as_secs_f64()
    );

    Ok(())
}
