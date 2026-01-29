mod model;

mod circuit_tests;
mod cumulative_tests;
mod linear_tests;

use std::{fs::File, io::BufReader};

pub use cumulative_tests::set_up_cumulative_state;
use drcp_format::reader::ProofReader;
pub use linear_tests::set_up_linear_leq_state;
use pumpkin_core::propagation::PropagatorConstructor;

use crate::propagators::model::{Constraint, Model};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Propagator {
    Linear,
    Cumulative,
    Circuit,
    AllDifferent,
}

pub(crate) struct Instances {
    model: Model,
    reader: ProofReader<BufReader<File>, i32>,
    propagator: Propagator,
}

impl Instances {
    pub fn new(propagator: Propagator) -> Instances {
        // Lookup the instances and proofs for the given propagator.
        todo!()
    }

    pub fn run_all<C>(&mut self, create_constructor: impl FnMut(Constraint) -> C)
    where
        C: PropagatorConstructor,
    {
        loop {
            let step = self
                .reader
                .next_step()
                .expect("proofs are readable and valid");

            let Some(step) = step else {
                break;
            };

            match step {
                drcp_format::Step::Inference(inference) => {
                    let label = inference
                        .label
                        .expect("all inferences have labels")
                        .as_ref();

                    let generated_by_constraint_id =
                        inference.generated_by.expect("all inferences have hints");
                    let generated_by = self
                        .model
                        .get_constraint(generated_by_constraint_id)
                        .expect("all proofs are valid");

                    match label {
                        "linear_bounds" if self.propagator == Propagator::Linear => {
                            todo!()
                        }

                        "time_table" if self.propagator == Propagator::Cumulative => {
                            todo!()
                        }

                        "all_different" if self.propagator == Propagator::AllDifferent => {
                            todo!()
                        }

                        // Skip label and propagator combinations that we do not care
                        // about.
                        _ => {}
                    }
                }

                // Only interested in inferences.
                drcp_format::Step::Deduction(_) | drcp_format::Step::Conclusion(_) => {}
            }
        }
    }
}
