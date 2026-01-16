use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::num::NonZero;
use std::path::Path;
use std::rc::Rc;
use std::time::Instant;

use drcp_format::ConstraintId;
use drcp_format::reader::ProofReader;

pub mod deductions;
pub mod inferences;
pub mod model;

pub(crate) mod math;

use model::*;

/// The errors that can be returned by the checker.
#[derive(Debug, thiserror::Error)]
pub enum CheckError {
    /// The inference with the given [`ConstraintId`] is invalid due to
    /// [`inferences::InvalidInference`].
    #[error("inference {0} is invalid: {1}")]
    InvalidInference(ConstraintId, inferences::InvalidInference),

    /// The inference with the given [`ConstraintId`] is invalid due to
    /// [`deductions::InvalidDeduction`].
    #[error("deduction {0} is invalid: {1}")]
    InvalidDeduction(ConstraintId, deductions::InvalidDeduction),

    /// The proof did not contain a conclusion line.
    #[error("the proof was not terminated with a conclusion")]
    MissingConclusion,

    /// The conclusion does not follow from any deduction in the proof.
    #[error("the conclusion is not present as a proof step")]
    InvalidConclusion,

    /// An I/O error prevented us from reading all the input.
    #[error("failed to read next proof line: {0}")]
    ProofReadError(#[from] drcp_format::reader::Error),
}

pub fn run_checker(
    model_path: impl AsRef<Path>,
    proof_path: impl AsRef<Path>,
) -> anyhow::Result<()> {
    let parse_start = Instant::now();
    let model = parse_model(model_path)?;
    println!("parse-flatzinc: {}s", parse_start.elapsed().as_secs_f32());

    let proof_reader = create_proof_reader(proof_path)?;
    println!("parse-proof: 0s");

    let verify_start = Instant::now();

    verify_proof(model, proof_reader).inspect_err(|err| {
        print_check_error_info(err);
        println!("validate: {}s", verify_start.elapsed().as_secs_f32());
    })?;

    println!("validate: {}s", verify_start.elapsed().as_secs_f32());

    println!("Proof is valid!");

    Ok(())
}

/// If the error is an invalid deduction, here we print additional info why the deduction is
/// invalid. In particular, it prints any inferences which were ignored because the premise was not
/// satisfied.
fn print_check_error_info(error: &CheckError) {
    let CheckError::InvalidDeduction(
        constraint_id,
        deductions::InvalidDeduction::NoConflict(unused_inferences),
    ) = error
    else {
        return;
    };

    eprintln!("Deduction {constraint_id} is invalid.");

    if unused_inferences.is_empty() {
        eprintln!("  Failed to derive conflict after applying all inferences.");
    } else {
        eprintln!("  Could not apply the following inferences:");

        for unused_inference in unused_inferences {
            let deductions::IgnoredInference {
                constraint_id,
                unsatisfied_premises,
            } = unused_inference;

            eprint!("  - {constraint_id}:");

            for premise in unsatisfied_premises {
                eprint!(" {premise}");
            }

            eprintln!();
        }
    }
}

/// The constraints supported by the checker.
#[derive(Debug, fzn_rs::FlatZincConstraint)]
enum FlatZincConstraints {
    #[name("int_lin_le")]
    LinearLeq {
        weights: fzn_rs::ArrayExpr<i32>,
        variables: fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>,
        bound: i32,
    },
    #[name("int_lin_eq")]
    LinearEq {
        weights: fzn_rs::ArrayExpr<i32>,
        variables: fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>,
        bound: i32,
    },
    #[name("pumpkin_cumulative")]
    Cumulative {
        start_times: fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>,
        durations: fzn_rs::ArrayExpr<i32>,
        resource_usages: fzn_rs::ArrayExpr<i32>,
        capacity: i32,
    },
    #[name("pumpkin_all_different")]
    AllDifferent(fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>),
}

type FlatZincModel = fzn_rs::TypedInstance<i32, FlatZincConstraints>;

/// Parse a FlatZinc file to a checker [`Model`].
fn parse_model(path: impl AsRef<Path>) -> anyhow::Result<Model> {
    let model_source = std::fs::read_to_string(path)?;

    // TODO: For now the error handling shortcuts here. Ideally the `FznError` type returns
    // something that can be converted to an owned type, but for now we have to work around the
    // error holding a reference to the source.
    let fzn_ast = fzn_rs::fzn::parse(&model_source).map_err(|err| anyhow::anyhow!("{err}"))?;

    let fzn_model = FlatZincModel::from_ast(fzn_ast)?;

    let mut model = Model::default();
    model.objective = match &fzn_model.solve.method.node {
        fzn_rs::Method::Satisfy => None,
        fzn_rs::Method::Optimize {
            direction: fzn_rs::ast::OptimizationDirection::Minimize,
            objective,
        } => Some(Objective::Minimize(objective.clone().into())),
        fzn_rs::Method::Optimize {
            direction: fzn_rs::ast::OptimizationDirection::Maximize,
            objective,
        } => Some(Objective::Maximize(objective.clone().into())),
    };

    for (name, variable) in fzn_model.variables.iter() {
        model.add_variable(Rc::clone(name), variable.domain.node.clone());
    }

    for (idx, annotated_constraint) in fzn_model.constraints.iter().enumerate() {
        let constraint_id = NonZero::new(idx as u32 + 1).expect(
            "we always add one, and idx is at least zero, constraint_id is always non-zero",
        );

        let constraint = match &annotated_constraint.constraint.node {
            FlatZincConstraints::LinearLeq {
                weights,
                variables,
                bound,
            } => {
                let weights = fzn_model.resolve_array(weights)?;
                let variables = fzn_model.resolve_array(variables)?;

                let mut terms = vec![];

                for (weight, variable) in weights.zip(variables) {
                    let weight = weight?;
                    let variable = variable?;

                    terms.push(Term {
                        weight: weight
                            .try_into()
                            .expect("flatzinc does not have 0-weight terms"),
                        variable: variable.into(),
                    });
                }

                Constraint::LinearLeq(Linear {
                    terms,
                    bound: *bound,
                })
            }

            FlatZincConstraints::LinearEq {
                weights,
                variables,
                bound,
            } => {
                let weights = fzn_model.resolve_array(weights)?;
                let variables = fzn_model.resolve_array(variables)?;

                let mut terms = vec![];

                for (weight, variable) in weights.zip(variables) {
                    let weight = weight?;
                    let variable = variable?;

                    terms.push(Term {
                        weight: weight
                            .try_into()
                            .expect("flatzinc does not have 0-weight terms"),
                        variable: variable.into(),
                    });
                }

                Constraint::LinearEq(Linear {
                    terms,
                    bound: *bound,
                })
            }

            FlatZincConstraints::Cumulative {
                start_times,
                durations,
                resource_usages,
                capacity,
            } => {
                let start_times = fzn_model.resolve_array(start_times)?;
                let durations = fzn_model.resolve_array(durations)?;
                let resource_usages = fzn_model.resolve_array(resource_usages)?;

                let tasks = start_times
                    .zip(durations)
                    .zip(resource_usages)
                    .map(
                        |((maybe_start_time, maybe_duration), maybe_resource_usage)| {
                            let start_time = maybe_start_time?;
                            let duration = maybe_duration?;
                            let resource_usage = maybe_resource_usage?;

                            Ok(Task {
                                start_time: start_time.into(),
                                duration,
                                resource_usage,
                            })
                        },
                    )
                    .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;

                Constraint::Cumulative(Cumulative {
                    tasks,
                    capacity: *capacity,
                })
            }

            FlatZincConstraints::AllDifferent(variables) => {
                let variables = fzn_model
                    .resolve_array(variables)?
                    .map(|maybe_variable| maybe_variable.map(Variable::from))
                    .collect::<Result<Vec<_>, _>>()?;

                Constraint::AllDifferent(AllDifferent { variables })
            }
        };

        let _ = model.add_constraint(constraint_id, constraint);
    }

    Ok(model)
}

/// Create a reader for the proof file.
///
/// GZipped proofs are decompressed on-demand.
fn create_proof_reader(
    path: impl AsRef<Path>,
) -> anyhow::Result<ProofReader<Box<dyn BufRead>, i32>> {
    let file = File::open(path.as_ref())?;

    if path.as_ref().extension().is_some_and(|ext| ext == "gz") {
        let decoder = flate2::read::GzDecoder::new(file);
        let buf_reader = BufReader::new(decoder);

        Ok(ProofReader::new(Box::new(buf_reader)))
    } else {
        let buf_reader = BufReader::new(file);

        Ok(ProofReader::new(Box::new(buf_reader)))
    }
}

/// Verify whether the given proof is valid w.r.t. the model.
pub fn verify_proof<Source: BufRead>(
    mut model: Model,
    mut proof: ProofReader<Source, i32>,
) -> Result<(), CheckError> {
    // To check a proof we iterate over every step.
    // - If the step is an inference, it is checked. If it is valid, then the inference is stored in
    //   the fact database to be used in the next deduction. Otherwise, an error is returned
    //   indicating that the step is invalid.
    // - If the step is a deduction, it is checked with respect to all the inferences in the fact
    //   database. If the deduction is valid, the fact database is cleared and the deduction is
    //   added to the model to be used in future inferences.

    let mut fact_database = BTreeMap::new();

    loop {
        let next_step = proof.next_step()?;

        let Some(step) = next_step else {
            // The loop stops when a conclusion is found, so at this point we know the proof does
            // not contain a conclusion.
            return Err(CheckError::MissingConclusion);
        };

        match step {
            drcp_format::Step::Inference(inference) => {
                let fact = inferences::verify_inference(&model, &inference)
                    .map_err(|err| CheckError::InvalidInference(inference.constraint_id, err))?;

                let _ = fact_database.insert(inference.constraint_id, fact);
            }

            drcp_format::Step::Deduction(deduction) => {
                let derived_constraint = deductions::verify_deduction(&deduction, &fact_database)
                    .map_err(|err| {
                    CheckError::InvalidDeduction(deduction.constraint_id, err)
                })?;

                let new_constraint_added = model.add_constraint(
                    deduction.constraint_id,
                    Constraint::Nogood(derived_constraint),
                );

                if !new_constraint_added {
                    return Err(CheckError::InvalidDeduction(
                        deduction.constraint_id,
                        deductions::InvalidDeduction::DuplicateConstraintId(
                            deduction.constraint_id,
                        ),
                    ));
                }

                // Forget the stored inferences.
                fact_database.clear();
            }

            drcp_format::Step::Conclusion(conclusion) => {
                if verify_conclusion(&model, &conclusion) {
                    return Ok(());
                } else {
                    return Err(CheckError::InvalidConclusion);
                }
            }
        }
    }
}

fn verify_conclusion(model: &Model, conclusion: &drcp_format::Conclusion<Rc<str>, i32>) -> bool {
    // First we ensure the conclusion type matches the solve item in the model.
    match (&model.objective, conclusion) {
        (Some(_), drcp_format::Conclusion::Unsat)
        | (None, drcp_format::Conclusion::DualBound(_)) => return false,

        _ => {}
    }

    // We iterate in reverse order, since it is likely that the conclusion is based on a constraint
    // towards the end of the proof.
    model.iter_constraints().rev().any(|(_, constraint)| {
        let Constraint::Nogood(nogood) = constraint else {
            return false;
        };

        match conclusion {
            drcp_format::Conclusion::Unsat => nogood.as_ref().is_empty(),
            drcp_format::Conclusion::DualBound(atomic) => {
                nogood.as_ref() == [Atomic::from(!atomic.clone())]
            }
        }
    })
}
