mod model;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use clap::Parser;
use drcp_format::reader::ProofReader;
use drcp_format::writer::ProofWriter;
use pumpkin_core::containers::HashMap;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof_processor::ProofProcessor;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::TransformableVariable;
use pumpkin_core::Solver;

use crate::model::FlatZincConstraints;
use crate::model::FlatZincModel;

#[derive(Parser)]
struct Cli {
    /// Path to the model file (.fzn).
    model_path: PathBuf,

    /// Path to the scaffold file.
    scaffold_path: PathBuf,

    /// Path to the full proof (output).
    full_proof_path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let cli = Cli::parse();

    let proof_processor = parse_model(&cli.model_path)?;
    let proof_reader = create_proof_reader(&cli.scaffold_path)?;
    let proof_writer = create_proof_writer(&cli.full_proof_path)?;

    proof_processor.process(proof_reader, proof_writer)?;

    Ok(())
}

fn parse_model(path: impl AsRef<Path>) -> anyhow::Result<ProofProcessor> {
    let model_source = std::fs::read_to_string(path)?;

    // TODO: For now the error handling shortcuts here. Ideally the `FznError` type returns
    // something that can be converted to an owned type, but for now we have to work around the
    // error holding a reference to the source.
    let fzn_ast = fzn_rs::fzn::parse(&model_source).map_err(|err| anyhow::anyhow!("{err}"))?;

    let fzn_model = FlatZincModel::from_ast(fzn_ast)?;

    let mut solver = Solver::default();
    let mut variable_map = HashMap::default();
    let mut constant_map = HashMap::default();

    // Create all the flatzinc variables in the solver.
    for (name, variable) in fzn_model.variables.iter() {
        let domain_id = model::create_domain_for_variable(&mut solver, name, variable);
        let _ = variable_map.insert(Rc::clone(name), domain_id);
    }

    // Post all the constraints to the solver.
    let mut is_consistent = true;

    for annotated_constraint in fzn_model.constraints.iter() {
        let constraint_tag = solver.new_constraint_tag();

        // If the solver is no-longer consistent, we still want to reserve constraint tags for the
        // remaining flatzinc constraints. The processor expects the empty nogood to have a
        // constraint tag that follows after _all_ the flatzinc constraints have one.

        if is_consistent {
            is_consistent &= post_constraint(
                &fzn_model,
                &mut solver,
                &mut constant_map,
                &mut variable_map,
                constraint_tag,
                &annotated_constraint.constraint.node,
            )?;
        }
    }

    Ok(ProofProcessor::from(solver))
}

/// Post the constraint to the solver.
///
/// If the solver is in an inconsistent state after adding this constraint, return `Ok(false)`.
fn post_constraint(
    fzn_model: &FlatZincModel,
    solver: &mut Solver,
    constant_map: &mut HashMap<i32, DomainId>,
    variable_map: &mut HashMap<Rc<str>, DomainId>,
    constraint_tag: ConstraintTag,
    constraint: &FlatZincConstraints,
) -> anyhow::Result<bool> {
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
                let variable = match variable? {
                    fzn_rs::VariableExpr::Identifier(name) => *variable_map.get(&name).unwrap(),
                    fzn_rs::VariableExpr::Constant(value) => *constant_map
                        .entry(value)
                        .or_insert_with(|| solver.new_bounded_integer(value, value)),
                };

                terms.push(variable.scaled(weight));
            }

            if solver
                .add_constraint(pumpkin_core::constraints::less_than_or_equals(
                    terms,
                    *bound,
                    constraint_tag,
                ))
                .post()
                .is_err()
            {
                return Ok(false);
            }
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
                let variable = match variable? {
                    fzn_rs::VariableExpr::Identifier(name) => *variable_map.get(&name).unwrap(),
                    fzn_rs::VariableExpr::Constant(value) => *constant_map
                        .entry(value)
                        .or_insert_with(|| solver.new_bounded_integer(value, value)),
                };

                terms.push(variable.scaled(weight));
            }

            if solver
                .add_constraint(pumpkin_core::constraints::equals(
                    terms,
                    *bound,
                    constraint_tag,
                ))
                .post()
                .is_err()
            {
                return Ok(false);
            }
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
                    let domain_id = match variable? {
                        fzn_rs::VariableExpr::Identifier(name) => *variable_map.get(&name).unwrap(),
                        fzn_rs::VariableExpr::Constant(value) => *constant_map
                            .entry(value)
                            .or_insert_with(|| solver.new_bounded_integer(value, value)),
                    };

                    Ok(domain_id)
                })
                .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;
            let durations = fzn_model
                .resolve_array(durations)?
                .collect::<Result<Vec<_>, _>>()?;
            let resource_usages = fzn_model
                .resolve_array(resource_usages)?
                .collect::<Result<Vec<_>, _>>()?;

            if solver
                .add_constraint(pumpkin_core::constraints::cumulative(
                    start_times,
                    durations,
                    resource_usages,
                    *capacity,
                    constraint_tag,
                ))
                .post()
                .is_err()
            {
                return Ok(false);
            }
        }

        FlatZincConstraints::AllDifferent(variables) => {
            let variables = fzn_model
                .resolve_array(variables)?
                .map(|variable| {
                    let domain_id = match variable? {
                        fzn_rs::VariableExpr::Identifier(name) => *variable_map.get(&name).unwrap(),
                        fzn_rs::VariableExpr::Constant(value) => *constant_map
                            .entry(value)
                            .or_insert_with(|| solver.new_bounded_integer(value, value)),
                    };

                    Ok(domain_id)
                })
                .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;

            if solver
                .add_constraint(pumpkin_core::constraints::all_different(
                    variables,
                    constraint_tag,
                ))
                .post()
                .is_err()
            {
                return Ok(false);
            }
        }
    }

    Ok(true)
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
