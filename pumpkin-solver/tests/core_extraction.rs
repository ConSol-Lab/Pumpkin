#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use pumpkin_conflict_resolvers::resolvers::ResolutionResolver;
use pumpkin_core::Solver;
use pumpkin_core::predicate;
use pumpkin_core::results::SatisfactionResultUnderAssumptions;
use pumpkin_core::termination::Indefinite;

#[test]
fn basic_core_extraction() {
    // We create the solver with default options
    let mut solver = Solver::default();

    // We create 3 variables with domains within the range [0, 10]
    let x = solver.new_bounded_integer(0, 2);
    let y = solver.new_bounded_integer(0, 2);
    let z = solver.new_bounded_integer(0, 2);

    // All constraints require a constraint tag.
    let constraint_tag = solver.new_constraint_tag();

    // We create the all-different constraint
    let _ = solver
        .add_constraint(pumpkin_constraints::all_different(
            vec![x, y, z],
            constraint_tag,
        ))
        .post();

    // We create a termination condition which allows the solver to run indefinitely
    let mut termination = Indefinite;
    // And we create a search strategy (in this case, simply the default)
    let mut brancher = solver.default_brancher();

    let mut resolver = ResolutionResolver::default();

    // Then we solve to satisfaction
    let assumptions = vec![predicate!(x == 1), predicate!(y <= 1), predicate!(y != 0)];
    let result = solver.satisfy_under_assumptions(
        &mut brancher,
        &mut termination,
        &mut resolver,
        &assumptions,
    );

    if let SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(mut unsatisfiable) =
        result
    {
        let core = unsatisfiable.extract_core();

        // In this case, the core should be equal to all assumption predicates
        assert_eq!(
            core,
            vec![predicate!(x == 1), predicate!(y <= 1), predicate!(y != 0)].into()
        );
    }
}
