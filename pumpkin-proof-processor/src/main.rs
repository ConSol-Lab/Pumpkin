mod deduction_propagator;
mod explanation_graph;
mod model;
mod predicate_heap;
mod processor;
mod variables;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use anyhow::Context;
use clap::Parser;
use clap_verbosity_flag::InfoLevel;
use clap_verbosity_flag::Verbosity;
use drcp_format::reader::ProofReader;
use drcp_format::writer::ProofWriter;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::state::State;
use pumpkin_core::variables::TransformableVariable;
use pumpkin_propagators::arithmetic::BinaryNotEqualsPropagatorArgs;
use pumpkin_propagators::arithmetic::LinearLessOrEqualPropagatorArgs;
use pumpkin_propagators::cumulative::ArgTask;
use pumpkin_propagators::cumulative::options::CumulativePropagatorOptions;
use pumpkin_propagators::cumulative::time_table::TimeTableOverIntervalPropagator;

use crate::model::FlatZincConstraints;
use crate::model::FlatZincModel;
use crate::processor::ProofProcessor;
use crate::variables::Variables;

#[derive(Parser)]
struct Cli {
    #[command(flatten)]
    verbose: Verbosity<InfoLevel>,

    /// Path to the model file (.fzn).
    model_path: PathBuf,

    /// Path to the scaffold file.
    scaffold_path: PathBuf,

    /// Path to the full proof (output).
    full_proof_path: PathBuf,

    /// Eliminate the variables annotated with `is_defined_var` (except the objective variable)
    /// from the inferences of every deduction, by introducing linking nogoods.
    ///
    /// The scaffold must have been produced with `--eliminate-defined-variables`, so that no
    /// deduction contains these variables.
    #[arg(long)]
    eliminate_defined_variables: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    env_logger::Builder::new()
        .format_timestamp(None)
        .format_target(false)
        .filter_level(cli.verbose.log_level_filter())
        .target(env_logger::Target::Stdout)
        .init();

    let proof_processor = parse_model(&cli.model_path, cli.eliminate_defined_variables)
        .with_context(|| "Failed to parse model")?;
    let proof_reader =
        create_proof_reader(&cli.scaffold_path).with_context(|| "Failed to read proof")?;
    let proof_writer = create_proof_writer(&cli.full_proof_path)
        .with_context(|| "Failed to create proof writer")?;

    proof_processor.process(proof_reader, proof_writer)?;

    Ok(())
}

fn parse_model(
    path: impl AsRef<Path>,
    eliminate_defined_variables: bool,
) -> anyhow::Result<ProofProcessor> {
    let model_source = std::fs::read_to_string(path)?;

    // TODO: For now the error handling shortcuts here. Ideally the `FznError` type returns
    // something that can be converted to an owned type, but for now we have to work around the
    // error holding a reference to the source.
    let fzn_ast = fzn_rs::fzn::parse(&model_source).map_err(|err| anyhow::anyhow!("{err}"))?;

    // The typed model does not keep the variable annotations.
    let auxiliary_variable_names = if eliminate_defined_variables {
        collect_auxiliary_variable_names(&fzn_ast)
    } else {
        vec![]
    };

    let fzn_model = FlatZincModel::from_ast(fzn_ast)?;

    let mut state = State::default();
    let mut variable_map = Variables::default();

    // Create all the flatzinc variables in the solver.
    for (name, variable) in fzn_model.variables.iter() {
        let domain_id = model::create_domain_for_variable(&mut state, name, variable);
        variable_map.add_variable(Rc::clone(name), domain_id);
    }

    for (idx, annotated_constraint) in fzn_model.constraints.iter().enumerate() {
        let constraint_tag = state.new_constraint_tag();
        assert_eq!(
            idx,
            constraint_tag.index(),
            "constraint tags for model constraints must be consecutive"
        );

        // If the solver is no-longer consistent, we still want to reserve constraint tags for the
        // remaining flatzinc constraints. The processor expects the empty nogood to have a
        // constraint tag that follows after _all_ the flatzinc constraints have one.

        post_constraint(
            &fzn_model,
            &mut state,
            &mut variable_map,
            constraint_tag,
            &annotated_constraint.constraint.node,
        )?;
    }

    let auxiliary_variables = auxiliary_variable_names
        .iter()
        .map(|name| {
            variable_map
                .get_domain_by_name(name)
                .expect("every variable of the model has a domain")
        })
        .collect();

    let mut proof_processor = ProofProcessor::new(state, variable_map);
    proof_processor.set_auxiliary_variables(auxiliary_variables);
    Ok(proof_processor)
}

/// Returns the names of the variables annotated with `is_defined_var`, except the objective
/// variable; the solver never forbids the objective variable.
fn collect_auxiliary_variable_names(fzn_ast: &fzn_rs::ast::Ast) -> Vec<Rc<str>> {
    let objective_name = match &fzn_ast.solve.method.node {
        fzn_rs::ast::Method::Optimize {
            objective: fzn_rs::ast::Literal::Identifier(name),
            ..
        } => Some(Rc::clone(name)),
        _ => None,
    };

    fzn_ast
        .variables
        .iter()
        .filter(|(name, variable)| {
            Some(*name) != objective_name.as_ref()
                && variable
                    .node
                    .annotations
                    .iter()
                    .any(|annotation| annotation.node.name() == "is_defined_var")
        })
        .map(|(name, _)| Rc::clone(name))
        .collect()
}

/// Post the constraint to the state.
fn post_constraint(
    fzn_model: &FlatZincModel,
    state: &mut State,
    variable_map: &mut Variables,
    constraint_tag: ConstraintTag,
    constraint: &FlatZincConstraints,
) -> anyhow::Result<()> {
    match constraint {
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
                let variable = variable_map.resolve(variable?, state);

                terms.push(variable.scaled(weight));
            }

            let _ = state.add_propagator(LinearLessOrEqualPropagatorArgs {
                x: terms.into(),
                c: *bound,
                constraint_tag,
            });
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
                let variable = variable_map.resolve(variable?, state);

                terms.push(variable.scaled(weight));
            }

            let negated_terms = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();

            let _ = state.add_propagator(LinearLessOrEqualPropagatorArgs {
                x: negated_terms,
                c: -*bound,
                constraint_tag,
            });

            let _ = state.add_propagator(LinearLessOrEqualPropagatorArgs {
                x: terms.into(),
                c: *bound,
                constraint_tag,
            });
        }

        FlatZincConstraints::Cumulative {
            start_times,
            durations,
            resource_usages,
            capacity,
        } => {
            let start_times = fzn_model
                .resolve_array(start_times)?
                .map(|variable| {
                    let domain_id = variable_map.resolve(variable?, state);
                    Ok(domain_id)
                })
                .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;
            let durations = fzn_model
                .resolve_array(durations)?
                .collect::<Result<Vec<_>, _>>()?;
            let resource_usages = fzn_model
                .resolve_array(resource_usages)?
                .collect::<Result<Vec<_>, _>>()?;

            let tasks = start_times
                .into_iter()
                .zip(durations)
                .zip(resource_usages)
                .map(|((start_time, processing_time), resource_usage)| ArgTask {
                    start_time,
                    processing_time,
                    resource_usage,
                })
                .collect::<Vec<_>>();

            let _ = state.add_propagator(TimeTableOverIntervalPropagator::new(
                &tasks,
                *capacity,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ));
        }

        FlatZincConstraints::AllDifferent(variables) => {
            let variables = fzn_model
                .resolve_array(variables)?
                .map(|variable| {
                    let domain_id = variable_map.resolve(variable?, state);
                    Ok(domain_id)
                })
                .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;

            for i in 0..variables.len() {
                for j in i + 1..variables.len() {
                    let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
                        a: variables[i],
                        b: variables[j],
                        constraint_tag,
                    });
                }
            }
        }
    }

    Ok(())
}

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

fn create_proof_writer(path: impl AsRef<Path>) -> anyhow::Result<ProofWriter<Box<dyn Write>, i32>> {
    let file = File::create(path.as_ref())?;

    if path.as_ref().extension().is_some_and(|ext| ext == "gz") {
        let encoder = flate2::write::GzEncoder::new(file, flate2::Compression::fast());
        let buf_writer = BufWriter::new(encoder);
        Ok(ProofWriter::new(Box::new(buf_writer)))
    } else {
        let buf_writer = BufWriter::new(file);
        Ok(ProofWriter::new(Box::new(buf_writer)))
    }
}
