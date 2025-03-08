use std::{num::NonZero, path::PathBuf};

use pumpkin_solver::{
    constraints, options::SolverOptions, predicate, proof::ProofLog, results::SatisfactionResult,
    termination::Indefinite, Solver,
};

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
