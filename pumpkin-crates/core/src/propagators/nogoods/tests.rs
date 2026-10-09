use crate::conjunction;
use crate::engine::test_solver::TestSolver;
use crate::predicate;
use crate::predicates::Predicate;

/// Add `nogood` as the solver adds a nogood from the model, together with its checkers.
fn add_nogood(solver: &mut TestSolver, nogood: Vec<Predicate>) {
    let constraint_tag = solver.state.new_constraint_tag();
    let propagation_mode = solver
        .state
        .get_propagator(solver.nogood_handle)
        .expect("the handle refers to the nogood propagator")
        .propagation_mode();
    let rule_checkers = propagation_mode.add_nogood_checkers(
        &mut solver.state,
        constraint_tag,
        &nogood,
        solver.nogood_handle.propagator_id(),
    );

    let (nogood_propagator, mut context) = solver
        .state
        .get_propagator_mut_with_context(solver.nogood_handle);
    nogood_propagator
        .expect("the handle refers to the nogood propagator")
        .add_nogood(nogood, rule_checkers, &mut context);
}

#[test]
fn ternary_nogood_propagate() {
    let mut solver = TestSolver::default();
    let dummy = solver.new_variable(0, 1);
    let a = solver.new_variable(1, 3);
    let b = solver.new_variable(-4, 4);
    let c = solver.new_variable(-10, 20);

    let id = solver.nogood_handle.propagator_id();

    let _ = solver.increase_lower_bound_and_notify(id, dummy.id(), dummy, 1);

    let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
    add_nogood(&mut solver, nogood.into());

    let _ = solver.increase_lower_bound_and_notify(id, a.id(), a, 3);
    let _ = solver.increase_lower_bound_and_notify(id, b.id(), b, 0);

    solver.propagate_until_fixed_point(id).expect("");

    let _ = solver.increase_lower_bound_and_notify(id, c.id(), c, 15);

    solver.propagate(id).expect("");

    assert_eq!(solver.upper_bound(b), 0);

    let reason_lb = solver.get_reason_int(predicate!(b <= 0));
    assert_eq!(conjunction!([a >= 2] & [c >= 10]), reason_lb);
}

#[test]
fn unsat() {
    let mut solver = TestSolver::default();
    let a = solver.new_variable(1, 3);
    let b = solver.new_variable(-4, 4);
    let c = solver.new_variable(-10, 20);

    let id = solver.nogood_handle.propagator_id();

    let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
    add_nogood(&mut solver, nogood.into());

    let _ = solver.increase_lower_bound_and_notify(id, a.id(), a, 3);
    let _ = solver.increase_lower_bound_and_notify(id, b.id(), b, 1);
    let _ = solver.increase_lower_bound_and_notify(id, c.id(), c, 15);

    let result = solver.propagate_until_fixed_point(id);
    assert!(result.is_err());
}
