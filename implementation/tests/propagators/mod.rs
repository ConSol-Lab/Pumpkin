mod model;
use implementation::propagators::cumulative::Task;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

mod circuit_tests;
mod cumulative_tests;
mod linear_tests;

use std::fmt::Display;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

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
use crate::propagators::model::Term;
use crate::propagators::model::parse_model;

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

#[derive(Debug, thiserror::Error)]
pub struct CheckerError<'a> {
    fact: Fact,
    instance: &'a str,
    propagator: Propagator,
}

impl<'a> Display for CheckerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Inference {:?} could not be checked by the checker for {:?} for the instance {:?}",
            self.fact, self.propagator, self.instance
        )
    }
}

pub(crate) fn run_instance(instance: &str, propagator: Propagator) -> Result<(), CheckerError<'_>> {
    let model_path =
        Path::new("../pumpkin-checker/tests/valid_proofs").join(format!("{instance}.fzn",));
    let model = parse_model(model_path.clone())
        .unwrap_or_else(|err| panic!("Failed to read model {model_path:?}\n{err:?}"));

    let proof_path =
        Path::new("../pumpkin-checker/tests/valid_proofs").join(format!("{instance}.drcp",));
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
                    "linear_bounds" if propagator == Propagator::Linear => match generated_by {
                        Constraint::LinearLeq(linear) => {
                            verify_linear_inference(linear, &fact, variable_state).map_err(
                                |_| CheckerError {
                                    fact,
                                    instance,
                                    propagator,
                                },
                            )?
                        }
                        Constraint::LinearEq(linear) => {
                            let try_upper_bound =
                                verify_linear_inference(linear, &fact, variable_state.clone());

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
                            let try_lower_bound =
                                verify_linear_inference(&inverted_linear, &fact, variable_state);

                            match (try_lower_bound, try_upper_bound) {
                                (Ok(_), Ok(_)) => panic!("This should not happen."),
                                (Ok(_), Err(_)) | (Err(_), Ok(_)) => {}
                                (Err(_), Err(_)) => {
                                    return Err(CheckerError {
                                        fact,
                                        instance,
                                        propagator,
                                    });
                                }
                            }
                        }
                        _ => unreachable!(),
                    },

                    "time_table" if propagator == Propagator::Cumulative => match generated_by {
                        Constraint::Cumulative(cumulative) => {
                            let checker = CumulativeChecker {
                                tasks: cumulative
                                    .tasks
                                    .iter()
                                    .map(|task| Task {
                                        start_time: task.start_time.clone(),
                                        duration: task
                                            .duration
                                            .try_into()
                                            .expect("Expected duration to be non-negative"),
                                        resource_usage: task
                                            .resource_usage
                                            .try_into()
                                            .expect("Expected resource usage to be non-negative"),
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
                                return Err(CheckerError {
                                    fact,
                                    instance,
                                    propagator,
                                });
                            }
                        }
                        _ => unreachable!(),
                    },

                    "all_different" if propagator == Propagator::AllDifferent => match generated_by
                    {
                        Constraint::AllDifferent(all_different) => {
                            let checker = AllDifferentChecker {
                                x: all_different.variables.clone(),
                            };

                            if checker.check(
                                variable_state,
                                &fact.premises,
                                fact.consequent.as_ref(),
                            ) {
                            } else {
                                return Err(CheckerError {
                                    fact,
                                    instance,
                                    propagator,
                                });
                            }
                        }
                        _ => unreachable!(),
                    },

                    "circuit" if propagator == Propagator::Circuit => match generated_by {
                        Constraint::Circuit(circuit) => {
                            let checker = CircuitChecker {
                                successors: circuit.successors.clone(),
                            };

                            if checker.check(
                                variable_state,
                                &fact.premises,
                                fact.consequent.as_ref(),
                            ) {
                            } else {
                                return Err(CheckerError {
                                    fact,
                                    instance,
                                    propagator,
                                });
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
