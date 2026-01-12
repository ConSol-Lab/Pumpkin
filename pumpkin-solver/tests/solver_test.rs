#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use std::path::PathBuf;

use pumpkin_conflict_resolvers::DefaultResolver;
use pumpkin_conflict_resolvers::resolvers::AnalysisMode;
use pumpkin_solver::Solver;
use pumpkin_solver::options::SolverOptions;
use pumpkin_solver::predicate;
use pumpkin_solver::proof::ProofLog;
use pumpkin_solver::results::SatisfactionResult;
use pumpkin_solver::termination::Indefinite;

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
        .post()
        .expect("no error");

    let _ = solver
        .add_constraint(pumpkin_constraints::not_equals(
            [variable],
            5,
            constraint_tag,
        ))
        .post()
        .expect_err("unsat");

    let mut brancher = solver.default_brancher();
    let mut resolver = DefaultResolver::new(AnalysisMode::OneUIP);

    let result = solver.satisfy(&mut brancher, &mut Indefinite, &mut resolver);
    assert!(matches!(result, SatisfactionResult::Unsatisfiable(_, _)));
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
        .post()
        .expect("no conflict");

    let _ = solver
        .add_constraint(pumpkin_constraints::less_than_or_equals(
            [x1],
            1,
            constraint_tag,
        ))
        .post()
        .expect_err("conflict");

    let mut brancher = solver.default_brancher();
    let mut resolver = DefaultResolver::new(AnalysisMode::OneUIP);

    let result = solver.satisfy(&mut brancher, &mut Indefinite, &mut resolver);
    assert!(matches!(result, SatisfactionResult::Unsatisfiable(_, _)));
}
