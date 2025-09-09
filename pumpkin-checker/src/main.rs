use std::fs::File;
use std::io::BufReader;
use std::num::NonZero;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::time::Instant;

use clap::Parser;
use drcp_format::reader::ProofReader;
use pumpkin_checker::model::Model;
use pumpkin_checker::model::Objective;
use pumpkin_checker::model::Task;
use pumpkin_checker::CheckError;
use pumpkin_checker::InvalidDeduction;

#[derive(Parser)]
struct Cli {
    /// Path to the model file (.fzn).
    model_path: PathBuf,

    /// Path to the proof file.
    proof_path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let parse_start = Instant::now();
    let model = parse_model(&cli.model_path)?;
    println!("parse-flatzinc: {}s", parse_start.elapsed().as_secs_f32());

    let proof_reader = create_proof_reader(&cli.proof_path)?;
    println!("parse-proof: 0s");

    let verify_start = Instant::now();
    let verification_result = pumpkin_checker::verify_proof(model, proof_reader);
    println!("validate: {}s", verify_start.elapsed().as_secs_f32());

    if let Err(CheckError::InvalidDeduction(
        constraint_id,
        InvalidDeduction::NoConflict(ref unused_inferences),
    )) = verification_result
    {
        eprintln!("Deduction {constraint_id} is invalid.");

        if unused_inferences.is_empty() {
            eprintln!("  Failed to derive conflict after applying all inferences.");
        } else {
            eprintln!("  Could not apply the following inferences:");

            for (inference_id, premises) in unused_inferences {
                eprint!("  - {inference_id}:");

                for premise in premises {
                    eprint!(" {premise}");
                }

                eprintln!();
            }
        }
    }

    verification_result?;

    println!("Proof is valid!");

    Ok(())
}

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
}

type FlatZincModel = fzn_rs::TypedInstance<i32, FlatZincConstraints>;

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
        } => Some(Objective::Minimize(objective.clone())),
        fzn_rs::Method::Optimize {
            direction: fzn_rs::ast::OptimizationDirection::Maximize,
            objective,
        } => Some(Objective::Maximize(objective.clone())),
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

                    terms.push((weight, variable));
                }

                pumpkin_checker::model::Constraint::LinearLeq(pumpkin_checker::model::Linear {
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

                    terms.push((weight, variable));
                }

                pumpkin_checker::model::Constraint::LinearEq(pumpkin_checker::model::Linear {
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
                                start_time,
                                duration,
                                resource_usage,
                            })
                        },
                    )
                    .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;

                pumpkin_checker::model::Constraint::Cumulative(pumpkin_checker::model::Cumulative {
                    tasks,
                    capacity: *capacity,
                })
            }
        };

        let _ = model.add_constraint(constraint_id, constraint);
    }

    Ok(model)
}

fn create_proof_reader(
    path: impl AsRef<Path>,
) -> anyhow::Result<ProofReader<BufReader<File>, i32>> {
    let file = File::open(path)?;
    let buf_reader = BufReader::new(file);

    Ok(ProofReader::new(buf_reader))
}
