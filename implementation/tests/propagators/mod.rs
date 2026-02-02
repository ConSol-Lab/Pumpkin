#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
mod model;
use implementation::propagators::cumulative::Task;
use implementation::propagators::linear::LinearConstructor;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
use pumpkin_core::TestSolver;
use pumpkin_core::containers::HashMap;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagatorId;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::TransformableVariable;

mod circuit_tests;
mod cumulative_tests;
mod linear_tests;

use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::rc::Rc;

pub use cumulative_tests::set_up_cumulative_state;
use drcp_format::reader::ProofReader;
use implementation::propagators::all_different::AllDifferentChecker;
use implementation::propagators::circuit::CircuitChecker;
use implementation::propagators::cumulative::CumulativeChecker;
use implementation::propagators::linear::LinearChecker;
pub use linear_tests::set_up_linear_leq_state;

use crate::propagators::model::Atomic;
use crate::propagators::model::Constraint;
use crate::propagators::model::Fact;
use crate::propagators::model::Linear;
use crate::propagators::model::Model;
use crate::propagators::model::Term;
use crate::propagators::model::parse_model;

struct ProofTestRunner<'a> {
    instance: &'a str,
    propagator: Propagator,

    run_checker: bool,
    check_conflicts: bool,
    check_propagations: bool,
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum CheckerError<'a> {
    #[error(
        "The {propagator:?} checker was not able to check the inference {fact:#?} for instance {instance}"
    )]
    CouldNotCheck {
        fact: Fact,
        instance: &'a str,
        propagator: Propagator,
    },
    #[error(
        "The {propagator:?} propagator was not able to recreate the conflict described by {fact:#?} for instance {instance}"
    )]
    ConflictCouldNotBeReproduced {
        fact: Fact,
        instance: &'a str,
        propagator: Propagator,
    },
    #[error(
        "The {propagator:?} propagator was not able to recreate the propagation described by {fact:#?} for instance {instance}"
    )]
    PropagationCouldNotBeReproduced {
        fact: Fact,
        instance: &'a str,
        propagator: Propagator,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Propagator {
    Linear,
    Cumulative,
    Circuit,
    AllDifferent,
}

const LINEAR_INSTANCES: [&str; 4] = [
    "market_split_u3_01",
    "market_split_u3_02",
    "market_split_u3_03",
    "market_split_u3_04",
];

#[allow(unused, reason = "Could be used in the future")]
const RCPSP_INSTANCE: [&str; 4] = ["rcpsp00", "rcpsp01", "rcpsp02", "rcpsp03"];

impl<'a> ProofTestRunner<'a> {
    pub(crate) fn new_runner(instance: &'a str, propagator: Propagator) -> Self {
        Self {
            instance,
            propagator,

            run_checker: true,
            check_conflicts: false,
            check_propagations: false,
        }
    }

    pub(crate) fn check_conflicts_only(mut self) -> Self {
        self.run_checker = false;
        self.check_conflicts = true;
        self.check_propagations = false;

        self
    }

    pub(crate) fn check_propagations_only(mut self) -> Self {
        self.run_checker = false;
        self.check_conflicts = false;
        self.check_propagations = true;

        self
    }

    pub(crate) fn run(&self) -> Result<(), CheckerError<'_>> {
        let model_path = Path::new("../pumpkin-checker/tests/valid_proofs")
            .join(format!("{}.fzn", self.instance));
        let model = parse_model(model_path.clone())
            .unwrap_or_else(|err| panic!("Failed to read model {model_path:?}\n{err:?}"));

        let proof_path = Path::new("../pumpkin-checker/tests/valid_proofs")
            .join(format!("{}.drcp", self.instance));
        let mut reader: ProofReader<BufReader<File>, i32> = ProofReader::new(BufReader::new(
            File::open(proof_path.clone())
                .unwrap_or_else(|_| panic!("Expected instance {proof_path:?} to exist")),
        ));

        loop {
            let step = reader.next_step().expect("proofs are readable and valid");

            let Some(step) = step else {
                break;
            };

            match step {
                drcp_format::Step::Inference(inference) => {
                    let label = inference.label.expect("all inferences have labels");

                    // We do not check initial domains
                    if label.as_ref() == "initial_domain" || label.as_ref() == "nogood" {
                        continue;
                    }

                    let generated_by_constraint_id =
                        inference.generated_by.expect("all inferences have hints");

                    let generated_by = model
                        .get_constraint(generated_by_constraint_id)
                        .expect("all proofs are valid");

                    let fact = Fact {
                        premises: inference.premises.iter().cloned().map(Into::into).collect(),
                        consequent: inference.consequent.clone().map(Into::into),
                    };

                    // Setup the state for a conflict check.
                    let variable_state = VariableState::prepare_for_conflict_check(
                        fact.premises.clone(),
                        fact.consequent.clone(),
                    )
                    .expect("Premises were inconsistent");

                    match label.as_ref() {
                        "linear_bounds" if self.propagator == Propagator::Linear => {
                            match generated_by {
                                Constraint::LinearLeq(linear) => {
                                    if self.run_checker {
                                        Self::verify_linear_inference(
                                            linear,
                                            &fact,
                                            variable_state,
                                        )
                                        .map_err(|_| {
                                            CheckerError::CouldNotCheck {
                                                fact: fact.clone(),
                                                instance: self.instance,
                                                propagator: self.propagator,
                                            }
                                        })?;
                                    }

                                    if self.check_conflicts && fact.consequent.is_none() {
                                        Self::recreate_conflict_linear(
                                            self.instance,
                                            linear,
                                            &fact,
                                            &model,
                                        )?;
                                    }

                                    if self.check_propagations && fact.consequent.is_some() {
                                        Self::recreate_propagation_linear(
                                            self.instance,
                                            linear,
                                            &fact,
                                            &model,
                                        )?;
                                    }
                                }
                                Constraint::LinearEq(linear) => {
                                    let inverted_linear = Linear {
                                        terms: linear
                                            .terms
                                            .iter()
                                            .map(|term| Term {
                                                weight: -term.weight,
                                                variable: term.variable.clone(),
                                            })
                                            .collect(),
                                        bound: -linear.bound,
                                    };

                                    if self.run_checker {
                                        let try_upper_bound = Self::verify_linear_inference(
                                            linear,
                                            &fact,
                                            variable_state.clone(),
                                        );
                                        let try_lower_bound = Self::verify_linear_inference(
                                            &inverted_linear,
                                            &fact,
                                            variable_state,
                                        );

                                        match (try_lower_bound, try_upper_bound) {
                                            (Ok(_), Ok(_)) => panic!("This should not happen."),
                                            (Ok(_), Err(_)) | (Err(_), Ok(_)) => {}
                                            (Err(_), Err(_)) => {
                                                return Err(CheckerError::CouldNotCheck {
                                                    fact,
                                                    instance: self.instance,
                                                    propagator: self.propagator,
                                                });
                                            }
                                        }
                                    }

                                    if self.check_propagations && fact.consequent.is_none() {
                                        let try_upper_bound = Self::recreate_conflict_linear(
                                            self.instance,
                                            linear,
                                            &fact,
                                            &model,
                                        );

                                        let try_lower_bound = Self::recreate_conflict_linear(
                                            self.instance,
                                            &inverted_linear,
                                            &fact,
                                            &model,
                                        );

                                        match (try_lower_bound, try_upper_bound) {
                                            (Ok(_), Ok(_)) => panic!("This should not happen."),
                                            (Ok(_), Err(_)) | (Err(_), Ok(_)) => {}
                                            (error @ Err(_), Err(_)) => error?,
                                        }
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }

                        "time_table" if self.propagator == Propagator::Cumulative => {
                            match generated_by {
                                Constraint::Cumulative(cumulative) => {
                                    if self.run_checker {
                                        let checker = CumulativeChecker {
                                            tasks: cumulative
                                                .tasks
                                                .iter()
                                                .map(|task| {
                                                    Task {
                                        start_time: task.start_time.clone(),
                                        duration: task
                                            .duration
                                            .try_into()
                                            .expect("Expected duration to be non-negative"),
                                        resource_usage: task
                                            .resource_usage
                                            .try_into()
                                            .expect("Expected resource usage to be non-negative"),
                                    }
                                                })
                                                .collect(),
                                            capacity: cumulative
                                                .capacity
                                                .try_into()
                                                .expect("Expected non-negative capacity"),
                                        };

                                        if checker.check(
                                            variable_state,
                                            &fact.premises,
                                            fact.consequent.as_ref(),
                                        ) {
                                        } else {
                                            return Err(CheckerError::CouldNotCheck {
                                                fact,
                                                instance: self.instance,
                                                propagator: self.propagator,
                                            });
                                        }
                                    }

                                    if self.check_conflicts && fact.consequent.is_none() {
                                        todo!()
                                    }

                                    if self.check_propagations && fact.consequent.is_some() {
                                        todo!()
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }

                        "all_different" if self.propagator == Propagator::AllDifferent => {
                            match generated_by {
                                Constraint::AllDifferent(all_different) => {
                                    if self.run_checker {
                                        let checker = AllDifferentChecker {
                                            x: all_different.variables.clone(),
                                        };

                                        if checker.check(
                                            variable_state,
                                            &fact.premises,
                                            fact.consequent.as_ref(),
                                        ) {
                                        } else {
                                            return Err(CheckerError::CouldNotCheck {
                                                fact,
                                                instance: self.instance,
                                                propagator: self.propagator,
                                            });
                                        }
                                    }

                                    if self.check_conflicts && fact.consequent.is_none() {
                                        todo!()
                                    }

                                    if self.check_propagations && fact.consequent.is_some() {
                                        todo!()
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }

                        "circuit" if self.propagator == Propagator::Circuit => match generated_by {
                            Constraint::Circuit(circuit) => {
                                if self.run_checker {
                                    let checker = CircuitChecker {
                                        successors: circuit.successors.clone(),
                                    };

                                    if checker.check(
                                        variable_state,
                                        &fact.premises,
                                        fact.consequent.as_ref(),
                                    ) {
                                    } else {
                                        return Err(CheckerError::CouldNotCheck {
                                            fact,
                                            instance: self.instance,
                                            propagator: self.propagator,
                                        });
                                    }
                                }

                                if self.check_conflicts && fact.consequent.is_none() {
                                    todo!()
                                }

                                if self.check_propagations && fact.consequent.is_some() {
                                    todo!()
                                }
                            }
                            _ => unreachable!(),
                        },

                        // Skip label and propagator combinations that we do not care
                        // about.
                        _ => {}
                    }
                }

                // Only interested in inferences.
                drcp_format::Step::Deduction(_) | drcp_format::Step::Conclusion(_) => {}
            }
        }

        Ok(())
    }
}

impl<'a> ProofTestRunner<'a> {
    fn verify_linear_inference(
        linear: &Linear,
        fact: &Fact,
        state: VariableState<Atomic>,
    ) -> Result<(), ()> {
        let checker = LinearChecker {
            x: linear.terms.clone(),
            bound: linear.bound,
        };
        if checker.check(state, &fact.premises, fact.consequent.as_ref()) {
            Ok(())
        } else {
            Err(())
        }
    }

    fn atomic_to_predicate(atomic: &Atomic, var: &DomainId) -> Predicate {
        match atomic {
            Atomic::IntAtomic(int_atomic) => match int_atomic.comparison {
                drcp_format::IntComparison::GreaterEqual => {
                    predicate!(var >= int_atomic.value)
                }
                drcp_format::IntComparison::LessEqual => {
                    predicate!(var <= int_atomic.value)
                }
                drcp_format::IntComparison::Equal => predicate!(var == int_atomic.value),
                drcp_format::IntComparison::NotEqual => predicate!(var != int_atomic.value),
            },
            _ => {
                unreachable!()
            }
        }
    }

    fn create_solver_for_fact(
        fact: &Fact,
        model: &Model,
    ) -> (TestSolver, HashMap<Rc<str>, DomainId>) {
        let mut solver = TestSolver::default();

        let mut variables: HashMap<Rc<str>, DomainId> = HashMap::default();
        for atomic in &fact.premises {
            let identifier = atomic.identifier();

            if !variables.contains_key(&identifier) {
                let domain = model.get_domain(&identifier);

                let var = match domain {
                    fzn_rs::ast::Domain::UnboundedInt => unimplemented!(),
                    fzn_rs::ast::Domain::Int(range_list) => solver.new_variable(
                        (*range_list.lower_bound()) as i32,
                        (*range_list.upper_bound()) as i32,
                    ),
                    fzn_rs::ast::Domain::Bool => solver.new_variable(0, 1),
                };
                let _ = variables.insert(Rc::clone(&identifier), var);
            }

            let var = variables.get(&identifier).unwrap();
            let atomic_predicate = Self::atomic_to_predicate(atomic, var);

            let _ = solver
                .post(atomic_predicate)
                .expect("Expected that apply of predicate would not lead to a conflict");
        }

        (solver, variables)
    }
}

/// Methods for checking conflicts and propagations linear
impl<'a> ProofTestRunner<'a> {
    fn add_linear_propagator(
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

    fn recreate_propagation_linear(
        instance: &'a str,
        linear: &Linear,
        fact: &Fact,
        model: &Model,
    ) -> Result<(), CheckerError<'a>> {
        assert!(fact.consequent.is_some());
        let (mut solver, variables) = Self::create_solver_for_fact(fact, model);

        let result = Self::add_linear_propagator(&mut solver, &variables, linear, false);

        let var = variables
            .get(&fact.consequent.as_ref().unwrap().identifier())
            .expect("Expected variable to exist");
        let consequent = Self::atomic_to_predicate(fact.consequent.as_ref().unwrap(), var);

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

    fn recreate_conflict_linear(
        instance: &'a str,
        linear: &Linear,
        fact: &Fact,
        model: &Model,
    ) -> Result<(), CheckerError<'a>> {
        assert!(fact.consequent.is_none());
        let (mut solver, variables) = Self::create_solver_for_fact(fact, model);

        let result = Self::add_linear_propagator(&mut solver, &variables, linear, true);

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
}
