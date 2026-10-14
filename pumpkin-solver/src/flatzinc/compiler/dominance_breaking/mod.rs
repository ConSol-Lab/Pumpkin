//! Dual fixing and dominance breaking on the linear part of a model.
//!
//! Both rest on the same argument: a change applied to any solution gives a solution that is at
//! least as good, so the solutions it maps away from can be excluded. [`dual_fixing`] moves one
//! variable to a bound; [`dominance_order`] exchanges the values of two variables, where `x_j`
//! dominates `x_k` when exchanging their values, in any solution with `x_k > x_j`, gives a
//! solution that is at least as good. For each dominated pair the pass posts `x_j >= x_k`.
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
//! smaller weight. No variable is fixed: raising any of them can violate the capacity, and
//! lowering any of them reduces the profit.
//!
//! [`linear_model`] reads every constraint with an exact linear form ([`linear_forms`]) as linear
//! rows. Dual fixing runs first, and dominance is detected among the variables it leaves free.
//! That order is sound: after the fixings, the fixed variables are constants, and exchanging two
//! free variables does not change them.
//!
//! Under croc proof logging, each posted constraint is justified by a dominance step ([`proof`]),
//! and only the constraints the checker decides are posted.
//! Both passes are refused under other proof logging, including every DRCP proof, because the
//! posted constraints are not implied by the model; when all solutions of a satisfaction problem
//! are requested, because they remove solutions; and together with symmetry breaking, because
//! their joint soundness with lex-leader constraints is not established.

mod dominance_order;
mod dual_fixing;
mod linear_forms;
mod linear_model;
pub(super) mod proof;
#[cfg(test)]
mod tests;

use std::time::Instant;

use flatzinc::Goal;
use log::info;

use self::dual_fixing::FixedBound;
use self::linear_model::bounds;
use super::breaking_steps;
use super::context::CompilationContext;
use super::symmetry_breaking::canonical_form::ModelIndex;
use super::symmetry_breaking::canonical_form::VarKind;
use crate::core::constraints::Constraint;
use crate::core::predicate;
use crate::core::variables::Literal;
use crate::flatzinc::CompilationOptions;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::preamble::Posted;

/// Candidates with the same domain are compared pairwise; larger groups are skipped.
const MAX_GROUP: usize = 2000;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &CompilationOptions,
) -> Result<(), FlatZincError> {
    if !options.dominance_breaking && !options.dual_fixing {
        return Ok(());
    }
    let unsupported = |reason| {
        if options.dominance_breaking {
            FlatZincError::DominanceBreakingUnsupported(reason)
        } else {
            FlatZincError::DualFixingUnsupported(reason)
        }
    };
    if !options.proof_states_breaking_steps() {
        return Err(unsupported(
            "proof logging other than with --proof-format croc and the scaffold or block proof type",
        ));
    }
    if options.symmetry_breaking {
        return Err(unsupported("symmetry breaking"));
    }
    if options.all_solutions && matches!(ast.solve_item.goal, Goal::Satisfy) {
        return Err(unsupported(
            "enumerating all solutions of a satisfaction problem",
        ));
    }

    let started = Instant::now();
    // A model the pass cannot read is solved without it instead of being rejected.
    let (index, mut model, mut proof) = match ModelIndex::build_with(ast, context, true).and_then(
        |index| {
            let model = linear_model::LinearModel::extract(ast, &index)?;
            let proof = if context.solver.is_logging_proof() {
                Some(proof::DominanceProof::new(ast, &index, context)?)
            } else {
                None
            };
            Ok((index, model, proof))
        },
    ) {
        Ok(read) => read,
        Err(error) => {
            info!(
                "dominance breaking and dual fixing: skipped, the model could not be read: {error}"
            );
            return Ok(());
        }
    };

    let mut fixings = if options.dual_fixing {
        dual_fixing::fixings(&model)
    } else {
        vec![]
    };
    if let Some(proof) = &proof {
        fixings.retain(|&(var, _)| proof.can_fix(&index, var));
    }
    let fixed: std::collections::HashSet<_> = fixings.iter().map(|&(var, _)| var).collect();
    let num_candidates = model.candidates.len();
    model
        .candidates
        .retain(|candidate| !fixed.contains(&candidate.var));

    // Under proof logging, the pairs the checker decides, each with the shift of the objective
    // variable that repairs its definition, and an order in which `better` precedes `worse`.
    let mut pairs = vec![];
    let mut skipped = 0;
    if options.dominance_breaking {
        let (all_pairs, all_skipped) =
            dominance_order::pairs_to_post(&model, &index.variables, MAX_GROUP);
        skipped = all_skipped;
        pairs = all_pairs
            .into_iter()
            .filter_map(|(better, worse)| match &proof {
                Some(proof) => proof
                    .exchange_shift(&index, better, worse)
                    .map(|shift| (better, worse, shift)),
                None => Some((better, worse, 0)),
            })
            .collect();
    }
    let pair_order = if proof.is_some() {
        let edges: Vec<_> = pairs
            .iter()
            .map(|&(better, worse, _)| (better, worse))
            .collect();
        match proof::order_of_pairs(&edges) {
            Some(order) => order,
            None => {
                info!("dominance breaking: the pairs have a cycle, none is posted");
                pairs.clear();
                vec![]
            }
        }
    } else {
        vec![]
    };
    if let Some(proof) = &proof {
        proof.log_order(context, &index, &fixings, &pair_order);
    }

    if options.dual_fixing {
        for &(var, bound) in &fixings {
            let domain_id = index.domain_ids[var];
            let (lower, upper) = bounds(&index.variables[var]);
            let value = match bound {
                FixedBound::Lower => lower,
                FixedBound::Upper => upper,
            };
            let value = i32::try_from(value).expect("bounds come from i32 domains");
            let tag = context.solver.new_constraint_tag();
            let predicate = match bound {
                FixedBound::Lower => predicate![domain_id <= value],
                FixedBound::Upper => predicate![domain_id >= value],
            };
            if let Some(proof) = &proof {
                proof.log_fixing(context, &index, var, bound, tag);
                let atoms = proof::preamble_atoms(context, &[predicate]);
                breaking_steps::record_posted(context, tag, Posted::Clause(atoms));
            }
            context.solver.add_clause([predicate], tag);
        }
        info!(
            "dual fixing: {} candidates, {} variables fixed ({:.2}s)",
            num_candidates,
            fixings.len(),
            started.elapsed().as_secs_f64()
        );
    }

    if options.dominance_breaking {
        for &(better, worse, shift) in &pairs {
            let better_id = index.domain_ids[better];
            let worse_id = index.domain_ids[worse];
            let tag = context.solver.new_constraint_tag();
            if let Some(proof) = &mut proof {
                // The nogood `[worse >= 1] /\ [better <= 0]`, as the clause the proof adds.
                proof.log_exchange(context, &index, better, worse, shift, tag);
                let clause = [predicate![worse_id <= 0], predicate![better_id >= 1]];
                let atoms = proof::preamble_atoms(context, &clause);
                breaking_steps::record_posted(context, tag, Posted::Clause(atoms));
                context.solver.add_clause(clause, tag);
                continue;
            }
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
    }

    Ok(())
}
