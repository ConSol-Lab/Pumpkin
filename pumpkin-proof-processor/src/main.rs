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
use pumpkin_core::proof_processor::ProofProcessor;
use pumpkin_core::variables::TransformableVariable;
use pumpkin_core::Solver;

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

fn parse_model(path: impl AsRef<Path>) -> anyhow::Result<ProofProcessor> {
    let model_source = std::fs::read_to_string(path)?;

    // TODO: For now the error handling shortcuts here. Ideally the `FznError` type returns
    // something that can be converted to an owned type, but for now we have to work around the
    // error holding a reference to the source.
    let fzn_ast = fzn_rs::fzn::parse(&model_source).map_err(|err| anyhow::anyhow!("{err}"))?;

    let fzn_model = FlatZincModel::from_ast(fzn_ast)?;

    let mut solver = Solver::default();
    let mut variable_map = HashMap::new();
    let mut constant_map = HashMap::new();

    for (name, variable) in fzn_model.variables.iter() {
        match &variable.domain.node {
            fzn_rs::ast::Domain::UnboundedInt => todo!("unbounded integers are not supported yet"),
            fzn_rs::ast::Domain::Bool => todo!("boolean variables are not supported yet"),

            fzn_rs::ast::Domain::Int(domain) => {
                assert!(
                    domain.is_continuous(),
                    "sparse domains are not yet supported"
                );

                let domain_id = solver.new_named_bounded_integer(
                    *domain.lower_bound() as i32,
                    *domain.upper_bound() as i32,
                    name.as_ref(),
                );

                let _ = variable_map.insert(Rc::clone(name), domain_id);
            }
        }
    }

    for annotated_constraint in fzn_model.constraints.iter() {
        let constraint_tag = solver.new_constraint_tag();

        match &annotated_constraint.constraint.node {
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

                solver
                    .add_constraint(pumpkin_core::constraints::less_than_or_equals(
                        terms,
                        *bound,
                        constraint_tag,
                    ))
                    .post()
                    .unwrap();
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

                solver
                    .add_constraint(pumpkin_core::constraints::equals(
                        terms,
                        *bound,
                        constraint_tag,
                    ))
                    .post()
                    .unwrap();
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
                            fzn_rs::VariableExpr::Identifier(name) => {
                                *variable_map.get(&name).unwrap()
                            }
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

                solver
                    .add_constraint(pumpkin_core::constraints::cumulative(
                        start_times,
                        durations,
                        resource_usages,
                        *capacity,
                        constraint_tag,
                    ))
                    .post()
                    .unwrap();
            }

            FlatZincConstraints::AllDifferent(variables) => {
                let variables = fzn_model
                    .resolve_array(variables)?
                    .map(|variable| {
                        let domain_id = match variable? {
                            fzn_rs::VariableExpr::Identifier(name) => {
                                *variable_map.get(&name).unwrap()
                            }
                            fzn_rs::VariableExpr::Constant(value) => *constant_map
                                .entry(value)
                                .or_insert_with(|| solver.new_bounded_integer(value, value)),
                        };

                        Ok(domain_id)
                    })
                    .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;

                solver
                    .add_constraint(pumpkin_core::constraints::all_different(
                        variables,
                        constraint_tag,
                    ))
                    .post()
                    .unwrap();
            }
        };
    }

    Ok(ProofProcessor::from(solver))
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
