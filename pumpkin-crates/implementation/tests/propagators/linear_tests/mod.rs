#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
use implementation::propagators::linear_propagator::LinearConstructor;
use pumpkin_core::TestSolver;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagatorId;
use pumpkin_core::variables::AffineView;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::TransformableVariable;

mod linear_checker_tests;
mod linear_conflict_tests;
mod linear_propagation_tests;

fn set_up_linear_leq_state(
    variable_info: &[((i32, i32), i32, i32)],
    c: i32,
) -> (
    TestSolver,
    Result<PropagatorId, Conflict>,
    Vec<AffineView<DomainId>>,
) {
    let mut solver = TestSolver::default();

    let mut variables = Vec::new();

    for ((lb, ub), scale, offset) in variable_info.iter() {
        let domain_id = solver.new_variable(*lb, *ub);
        variables.push(domain_id.scaled(*scale).offset(*offset));
    }

    let constraint_tag = solver.new_constraint_tag();
    let result = solver.new_propagator(LinearConstructor {
        x: variables.clone().into(),
        c,
        constraint_tag,
    });
    (solver, result, variables)
}
