use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use anyhow::Context;
use clap::Parser;
use clap::Subcommand;
use drcp_format::reader::ProofReader;
use drcp_format::Conclusion;
use drcp_format::Step;
use itertools::Itertools;

#[derive(Parser)]
struct Cli {
    /// The input proof.
    input_proof: PathBuf,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Replace atomic codes with the actual atomics.
    InlineAtomics {
        /// Path to the model that the proof is for.
        ///
        /// If provided, the `generated_by` labels will be re-written to the actual
        /// constraint from the model.
        model: Option<PathBuf>,

        /// The output proof where literals are replaced with atomics.
        output: PathBuf,
    },

    /// Gather statistics on the proof.
    Statistics,
}

fn inline_atomics<Source: BufRead>(
    output_path: PathBuf,
    model_path: Option<PathBuf>,
    mut reader: ProofReader<Source, i32>,
) -> anyhow::Result<()> {
    // Parse the model in case it is provided.
    let model = if let Some(path) = model_path {
        let contents = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read model from '{}'", path.display()))?;

        let ast = fzn_rs::fzn::parse(&contents).unwrap();
        //            .with_context(|| format!("Failed to parse model in '{}'.", path.display()));

        Some(ast)
    } else {
        None
    };

    let mut output = File::create(&output_path)
        .with_context(|| format!("Failed to create {}", output_path.display()))?;

    while let Some(step) = reader.next_step()? {
        match step {
            Step::Inference(inference) => {
                write!(output, "i {}", inference.constraint_id)?;

                for premise in inference.premises {
                    write!(output, " {}", premise)?;
                }

                if let Some(propagated) = inference.consequent {
                    write!(output, " 0 {}", propagated)?;
                }

                if let Some(label) = inference.label {
                    write!(output, " l:{label}")?;
                }

                if let Some(constraint_id) = inference.generated_by {
                    if let Some(ref model) = model {
                        if let Some(constraint) =
                            model.constraints.get(constraint_id.get() as usize - 1)
                        {
                            #[allow(
                                unstable_name_collisions,
                                reason = "intersperse will take a while, precisely because it exists in itertools"
                            )]
                            write!(
                                output,
                                " =| {}({});",
                                constraint.node.name.node,
                                constraint
                                    .node
                                    .arguments
                                    .iter()
                                    .map(|argument| display_argument(&argument.node))
                                    .intersperse(", ".to_owned())
                                    .collect::<String>()
                            )?;
                        } else {
                            write!(output, " c:{constraint_id}")?;
                        }
                    } else {
                        write!(output, " c:{constraint_id}")?;
                    }
                }

                writeln!(output)?;
            }
            Step::Deduction(deduction) => {
                write!(output, "n {}", deduction.constraint_id)?;

                for premise in deduction.premises {
                    write!(output, " {}", premise)?;
                }

                write!(output, " 0")?;

                for hint in deduction.sequence {
                    write!(output, " {}", hint)?;
                }

                writeln!(output)?;
            }
            Step::Conclusion(conclusion) => match conclusion {
                Conclusion::Unsat => writeln!(output, "c UNSAT")?,
                Conclusion::DualBound(bound) => writeln!(output, "c {bound}")?,
            },
        }
    }

    Ok(())
}

fn display_argument(argument: &fzn_rs::ast::Argument) -> String {
    match argument {
        #[allow(
            unstable_name_collisions,
            reason = "intersperse will take a while, precisely because it exists in itertools"
        )]
        fzn_rs::ast::Argument::Array(nodes) => format!(
            "[{}]",
            nodes
                .iter()
                .map(|node| display_literal(&node.node))
                .intersperse(", ".to_owned())
                .collect::<String>()
        ),
        fzn_rs::ast::Argument::Literal(node) => display_literal(&node.node),
    }
}

fn display_literal(literal: &fzn_rs::ast::Literal) -> String {
    match literal {
        fzn_rs::ast::Literal::Int(int) => int.to_string(),
        fzn_rs::ast::Literal::Identifier(identifier) => identifier.as_ref().to_owned(),
        fzn_rs::ast::Literal::Bool(boolean) => boolean.to_string(),
        fzn_rs::ast::Literal::IntSet(_) => todo!("Display set literal"),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InferenceLabel {
    Unknown,
    Label(Rc<str>),
}

impl Display for InferenceLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferenceLabel::Unknown => write!(f, "unknown"),
            InferenceLabel::Label(label) => write!(f, "{label}"),
        }
    }
}

impl PartialEq<&'_ str> for InferenceLabel {
    fn eq(&self, other: &&str) -> bool {
        match self {
            InferenceLabel::Unknown => false,
            InferenceLabel::Label(label) => label.as_ref() == *other,
        }
    }
}

const TIME_TABLE_LABEL: &str = "time_table";

#[derive(Default)]
struct Statistics {
    num_deductions: usize,
    inference_types: HashMap<InferenceLabel, usize>,
    conclusion: Option<Conclusion<Rc<str>, i32>>,
    tasks_in_time_table_inference: OnlineStatistic,
    stage_size: OnlineStatistic,
}

/// Allows for online computation of mean and variance.
///
/// See http://datagenetics.com/blog/november22017/index.html
#[derive(Default)]
struct OnlineStatistic {
    num_samples: usize,
    mean: f64,
    variance_times_n: f64,
}

impl OnlineStatistic {
    fn add_sample(&mut self, sample: f64) {
        self.num_samples += 1;

        let previous_mean = self.mean;
        self.mean += (sample - previous_mean) / self.num_samples as f64;

        self.variance_times_n += (sample - previous_mean) * (sample - self.mean);
    }

    fn variance(&self) -> f64 {
        self.variance_times_n / self.num_samples as f64
    }

    fn stddev(&self) -> f64 {
        self.variance().sqrt()
    }
}

impl Statistics {
    fn num_inferences(&self) -> usize {
        self.inference_types.values().sum()
    }

    fn num_steps(&self) -> usize {
        self.num_deductions + self.num_inferences()
    }
}

fn gather_statistics<Source: BufRead>(mut reader: ProofReader<Source, i32>) -> anyhow::Result<()> {
    let mut statistics = Statistics::default();

    let mut stage_size = 0;

    while let Some(step) = reader.next_step()? {
        match step {
            Step::Inference(inference) => {
                stage_size += 1;

                let label = inference
                    .label
                    .map(InferenceLabel::Label)
                    .unwrap_or(InferenceLabel::Unknown);

                let counter = statistics.inference_types.entry(label.clone()).or_default();
                *counter += 1;

                if label == TIME_TABLE_LABEL {
                    let names = inference
                        .premises
                        .iter()
                        .chain(inference.consequent.as_ref())
                        .map(|atomic| &atomic.name)
                        .collect::<HashSet<_>>();

                    statistics
                        .tasks_in_time_table_inference
                        .add_sample(names.len() as f64);
                }
            }
            Step::Deduction(_) => {
                statistics.stage_size.add_sample(stage_size as f64);
                stage_size = 0;

                statistics.num_deductions += 1;
            }
            Step::Conclusion(conclusion) => {
                statistics.conclusion = Some(conclusion);
            }
        }
    }

    println!("num-steps: {}", statistics.num_steps());
    println!("num-deductions: {}", statistics.num_deductions);
    println!("num-inferences: {}", statistics.num_inferences());

    for (label, count) in statistics.inference_types.iter() {
        println!("num-inferences-{}: {}", label, count);
    }

    let online_statistics = [
        ("stage-size", &statistics.stage_size),
        (
            "tasks-in-time-table-inferences",
            &statistics.tasks_in_time_table_inference,
        ),
    ];

    for (name, statistic) in online_statistics {
        println!("mean-{name}: {}", statistic.mean);
        println!("stddev-{name}: {}", statistic.stddev());
    }

    match statistics.conclusion {
        Some(Conclusion::Unsat) => println!("conclusion: unsat"),
        Some(Conclusion::DualBound(_)) => println!("conclusion: dualbound"),
        None => println!("conclusion: none"),
    }

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    let reader = create_proof_reader(&args.input_proof)?;

    match args.command {
        Commands::InlineAtomics { output, model } => inline_atomics(output, model, reader)?,
        Commands::Statistics => gather_statistics(reader)?,
    }

    Ok(())
}

fn create_proof_reader(
    path: impl AsRef<Path>,
) -> anyhow::Result<ProofReader<Box<dyn BufRead>, i32>> {
    let file = File::open(path.as_ref())
        .with_context(|| format!("Failed to open {}", path.as_ref().display()))?;

    if path.as_ref().extension().is_some_and(|ext| ext == "gz") {
        let decoder = flate2::read::GzDecoder::new(file);
        let buf_reader = BufReader::new(decoder);

        Ok(ProofReader::new(Box::new(buf_reader)))
    } else {
        let buf_reader = BufReader::new(file);

        Ok(ProofReader::new(Box::new(buf_reader)))
    }
}
