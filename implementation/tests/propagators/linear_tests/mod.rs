#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
use std::rc::Rc;

use implementation::propagators::linear::LinearConstructor;
use pumpkin_checking::AtomicConstraint;
use pumpkin_core::TestSolver;
use pumpkin_core::containers::HashMap;
use pumpkin_core::predicates::PredicateConstructor;
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

pub(crate) fn invalidate_linear_fact(linear: &Linear, fact: &mut Fact, model: &Model) {
    let (solver, variables) = ProofTestRunner::create_solver_for_fact(fact, model);

    let lb: i32 = fact
        .premises
        .iter()
        .map(|premise| solver.lower_bound(*variables.get(&premise.identifier()).unwrap()))
        .sum();

    if let Some(consequent) = &fact.consequent {
        let consequent_var = variables.get(&consequent.identifier()).unwrap();
        let slack = (linear.bound - (lb + solver.lower_bound(*consequent_var))).abs() + 1;
        let reduction_per_var = (slack as f64 / fact.premises.len() as f64).ceil() as i32;

        fact.premises.iter_mut().for_each(|premise| {
            let term = linear
                .terms
                .iter()
                .find(|term| match &term.variable.0 {
                    fzn_rs::VariableExpr::Identifier(identifier) => {
                        *identifier == premise.identifier()
                    }
                    fzn_rs::VariableExpr::Constant(constant) => {
                        if premise.identifier().as_ref() == "true" {
                            *constant == 1
                        } else if premise.identifier().as_ref() == "false" {
                            *constant == 0
                        } else {
                            false
                        }
                    }
                })
                .unwrap();
            let id = premise.identifier();
            if let super::model::Atomic::IntAtomic(int_atomic) = premise {
                let var = variables.get(&id).unwrap().scaled(term.weight.into());
                let predicate =
                    var.lower_bound_predicate(solver.lower_bound(var) - reduction_per_var);

                if predicate.get_right_hand_side() == int_atomic.value {
                    if term.weight.is_positive() {
                        int_atomic.value -= 1
                    } else {
                        int_atomic.value += 1
                    }
                } else {
                    int_atomic.value = predicate.get_right_hand_side();
                }
            }
        })
    } else {
        let slack = (linear.bound - lb).abs() + 1;
        let reduction_per_var = (slack as f64 / fact.premises.len() as f64).ceil() as i32;

        fact.premises.iter_mut().for_each(|premise| {
            let term = linear
                .terms
                .iter()
                .find(|term| match &term.variable.0 {
                    fzn_rs::VariableExpr::Identifier(identifier) => {
                        *identifier == premise.identifier()
                    }
                    fzn_rs::VariableExpr::Constant(constant) => {
                        if premise.identifier().as_ref() == "true" {
                            *constant == 1
                        } else if premise.identifier().as_ref() == "false" {
                            *constant == 0
                        } else {
                            false
                        }
                    }
                })
                .unwrap();
            let id = premise.identifier();
            if let super::model::Atomic::IntAtomic(int_atomic) = premise {
                let var = variables.get(&id).unwrap().scaled(term.weight.into());
                let predicate =
                    var.lower_bound_predicate(solver.lower_bound(var) - reduction_per_var);

                if predicate.get_right_hand_side() == int_atomic.value {
                    if term.weight.is_positive() {
                        int_atomic.value -= 1
                    } else {
                        int_atomic.value += 1
                    }
                } else {
                    int_atomic.value = predicate.get_right_hand_side();
                }
            }
        })
    }
}
