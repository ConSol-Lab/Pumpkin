#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use std::path::PathBuf;

use pumpkin_branching::DefaultBrancher;
use pumpkin_conflict_resolvers::resolvers::ResolutionResolver;
use pumpkin_solver::Solver;
use pumpkin_solver::core::options::SolverOptions;
use pumpkin_solver::core::predicate;
use pumpkin_solver::core::proof::ProofLog;
use pumpkin_solver::core::results::SatisfactionResult;
use pumpkin_solver::core::termination::Indefinite;

#[test]
fn proof_with_reified_literals() {
    let mut solver = Solver::with_options(SolverOptions {
        proof_log: ProofLog::cp(&PathBuf::from("/tmp/solver_proof.drcp"), true)
            .expect("created proof"),
        ..Default::default()
    });

    let constraint_tag = solver.new_constraint_tag();
    let variable = solver.new_named_bounded_integer(1, 10, "var");
    let literal = solver.new_literal_for_predicate(predicate![variable == 5], constraint_tag);

    solver
        .add_constraint(pumpkin_constraints::clause(vec![literal], constraint_tag))
        .post();

    solver
        .add_constraint(pumpkin_constraints::not_equals(
            [variable],
            5,
            constraint_tag,
        ))
        .post();

    let mut brancher = DefaultBrancher::default_over_all_variables(&solver);
    let mut resolver = ResolutionResolver::default();

    let result = solver.satisfy(&mut brancher, &mut Indefinite, &mut resolver);
    assert!(matches!(result, SatisfactionResult::Unsatisfiable(_, _, _)));
}

#[test]
fn proof_with_equality_unit_nogood_step() {
    let mut solver = Solver::with_options(SolverOptions {
        proof_log: ProofLog::cp(&PathBuf::from("/tmp/solver_proof.drcp"), true)
            .expect("created proof"),
        ..Default::default()
    });

    let constraint_tag = solver.new_constraint_tag();

    let x1 = solver.new_named_bounded_integer(1, 2, "x1");
    let x2 = solver.new_named_bounded_integer(1, 1, "x2");
    solver
        .add_constraint(pumpkin_constraints::binary_not_equals(
            x1,
            x2,
            constraint_tag,
        ))
        .post();

    solver
        .add_constraint(pumpkin_constraints::less_than_or_equals(
            [x1],
            1,
            constraint_tag,
        ))
        .post();

    let mut brancher = DefaultBrancher::default_over_all_variables(&solver);
    let mut resolver = ResolutionResolver::default();

    let result = solver.satisfy(&mut brancher, &mut Indefinite, &mut resolver);
    assert!(matches!(result, SatisfactionResult::Unsatisfiable(_, _, _)));
}

/// Five pigeons in four holes, with the proof logged in the block format; the proof stops at
/// `deadline` when one is given.
fn pigeons_with_block_proof(
    proof_path: &std::path::Path,
    deadline: Option<pumpkin_solver::core::termination::Instant>,
) -> bool {
    let mut solver = Solver::with_options(SolverOptions {
        proof_log: ProofLog::blocks(proof_path, false, false, false, false).expect("created proof"),
        ..Default::default()
    });
    let constraint_tag = solver.new_constraint_tag();
    let pigeons: Vec<_> = (0..5)
        .map(|pigeon| solver.new_named_bounded_integer(1, 4, format!("p{pigeon}")))
        .collect();
    for (index, &first) in pigeons.iter().enumerate() {
        for &second in &pigeons[index + 1..] {
            solver
                .add_constraint(pumpkin_constraints::binary_not_equals(
                    first,
                    second,
                    constraint_tag,
                ))
                .post();
        }
    }
    if let Some(deadline) = deadline {
        solver.stop_proof_at(deadline);
    }

    let mut brancher = DefaultBrancher::default_over_all_variables(&solver);
    let mut resolver = ResolutionResolver::default();
    let result = solver.satisfy(&mut brancher, &mut Indefinite, &mut resolver);
    matches!(result, SatisfactionResult::Unsatisfiable(_, _, _))
}

#[test]
fn a_proof_past_its_deadline_is_truncated_and_the_search_goes_on() {
    let directory = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let whole_path = directory.join("pigeons_whole.croc");
    let truncated_path = directory.join("pigeons_truncated.croc");

    assert!(pigeons_with_block_proof(&whole_path, None));
    assert!(pigeons_with_block_proof(
        &truncated_path,
        Some(pumpkin_solver::core::termination::Instant::now())
    ));

    let whole = std::fs::read_to_string(&whole_path).expect("the proof is text");
    let truncated = std::fs::read_to_string(&truncated_path).expect("the proof is text");
    assert!(whole.lines().any(|line| line == "c UNSAT"));
    assert!(!truncated.lines().any(|line| line.starts_with("c ")));
    assert!(truncated.len() < whole.len());
}
