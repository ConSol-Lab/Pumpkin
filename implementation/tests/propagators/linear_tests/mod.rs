#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
use std::rc::Rc;

use implementation::propagators::linear::LinearConstructor;
use pumpkin_checking::AtomicConstraint;
use pumpkin_core::TestSolver;
use pumpkin_core::containers::HashMap;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagatorId;
use pumpkin_core::variables::AffineView;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::TransformableVariable;

use crate::CheckerError;
use crate::Propagator;
use crate::propagators::ProofTestRunner;
use crate::propagators::model::Fact;
use crate::propagators::model::Linear;
use crate::propagators::model::Model;

mod linear_checker_tests;
mod linear_conflict_tests;
mod linear_propagation_tests;

pub fn set_up_linear_leq_state(
    variable_info: &[((i32, i32), i32, i32)],
    c: i32,
    conflict_detection_only: bool,
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
        bound: c,
        constraint_tag,
        conflict_detection_only,
    });
    (solver, result, variables)
}

pub(crate) fn add_linear_propagator(
    solver: &mut TestSolver,
    variables: &HashMap<Rc<str>, DomainId>,
    linear: &Linear,
    conflict_detection_only: bool,
) -> Result<PropagatorId, Conflict> {
    let x = linear
        .terms
        .iter()
        .map(|term| {
            let identifier = &term.variable.0;
            match identifier {
                fzn_rs::VariableExpr::Identifier(ident) => variables
                    .get(ident)
                    .expect("Expected to be able to retrieve variable")
                    .scaled(term.weight.into()),
                fzn_rs::VariableExpr::Constant(constant) => {
                    solver.new_variable(*constant, *constant).scaled(1)
                }
            }
        })
        .collect::<Vec<_>>();

    let constraint_tag = solver.new_constraint_tag();
    solver.new_propagator(LinearConstructor {
        x: x.into(),
        bound: linear.bound,
        constraint_tag,
        conflict_detection_only,
    })
}

pub(crate) fn recreate_propagation_linear<'a>(
    instance: &'a str,
    linear: &Linear,
    fact: &Fact,
    model: &Model,
) -> Result<(), CheckerError<'a>> {
    assert!(fact.consequent.is_some());
    let (mut solver, variables) = ProofTestRunner::create_solver_for_fact(fact, model);

    let result = add_linear_propagator(&mut solver, &variables, linear, false);

    let var = variables
        .get(&fact.consequent.as_ref().unwrap().identifier())
        .expect("Expected variable to exist");
    let consequent = ProofTestRunner::atomic_to_predicate(fact.consequent.as_ref().unwrap(), var);

    if result.is_ok() && solver.is_predicate_satisfied(consequent) {
        // We have been able to reproduce the conflict
        Ok(())
    } else {
        Err(CheckerError::PropagationCouldNotBeReproduced {
            fact: fact.clone(),
            instance,
            propagator: Propagator::Linear,
        })
    }
}

pub(crate) fn recreate_conflict_linear<'a>(
    instance: &'a str,
    linear: &Linear,
    fact: &Fact,
    model: &Model,
) -> Result<(), CheckerError<'a>> {
    assert!(fact.consequent.is_none());
    let (mut solver, variables) = ProofTestRunner::create_solver_for_fact(fact, model);

    let result = add_linear_propagator(&mut solver, &variables, linear, true);

    if result.is_err() {
        // We have been able to reproduce the conflict
        Ok(())
    } else {
        Err(CheckerError::ConflictCouldNotBeReproduced {
            fact: fact.clone(),
            instance,
            propagator: Propagator::Linear,
        })
    }
}
