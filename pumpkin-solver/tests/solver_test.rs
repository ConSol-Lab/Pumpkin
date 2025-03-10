#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use std::num::NonZero;
use std::path::PathBuf;

use pumpkin_solver::constraints;
use pumpkin_solver::options::SolverOptions;
use pumpkin_solver::predicate;
use pumpkin_solver::proof::ProofLog;
use pumpkin_solver::results::SatisfactionResult;
use pumpkin_solver::termination::Indefinite;
use pumpkin_solver::Solver;

#[test]
fn proof_with_reified_literals() {
    let mut solver = Solver::with_options(SolverOptions {
        proof_log: ProofLog::cp(
            &PathBuf::from("/tmp/solver_proof.drcp"),
            drcp_format::Format::Text,
            true,
            true,
        )
        .expect("created proof"),
        ..Default::default()
    });
    let variable = solver.new_named_bounded_integer(1, 10, "var");
    let literal = solver.new_literal_for_predicate(predicate![variable == 5]);

    solver
        .add_constraint(constraints::clause(vec![literal]))
        .post()
        .expect("no error");

    let _ = solver
        .add_constraint(constraints::not_equals([variable], 5))
        .with_tag(NonZero::new(2).unwrap())
        .post()
        .expect_err("unsat");

    let mut brancher = solver.default_brancher();
    let result = solver.satisfy(&mut brancher, &mut Indefinite);
    assert!(matches!(result, SatisfactionResult::Unsatisfiable));
}

#[test]
fn proof_with_equality_unit_nogood_step() {
    let mut solver = Solver::with_options(SolverOptions {
        proof_log: ProofLog::cp(
            &PathBuf::from("/tmp/solver_proof.drcp"),
            drcp_format::Format::Text,
            true,
            true,
        )
        .expect("created proof"),
        ..Default::default()
    });

    let x1 = solver.new_named_bounded_integer(1, 2, "x1");
    let x2 = solver.new_named_bounded_integer(1, 1, "x2");
    solver
        .add_constraint(constraints::binary_not_equals(x1, x2))
        .with_tag(NonZero::new(1).unwrap())
        .post()
        .expect("no conflict");

    let _ = solver
        .add_constraint(constraints::less_than_or_equals([x1], 1))
        .with_tag(NonZero::new(2).unwrap())
        .post()
        .expect_err("conflict");

    let mut brancher = solver.default_brancher();
    let result = solver.satisfy(&mut brancher, &mut Indefinite);
    assert!(matches!(result, SatisfactionResult::Unsatisfiable));
}
