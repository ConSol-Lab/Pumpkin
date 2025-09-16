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
        /// The output proof where literals are replaced with atomics.
        output: PathBuf,
    },

    /// Gather statistics on the proof.
    Statistics,
}

fn inline_atomics<Source: BufRead>(
    output_path: PathBuf,
    mut reader: ProofReader<Source, i32>,
) -> anyhow::Result<()> {
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
                    write!(output, " c:{constraint_id}")?;
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
    sum_of_tasks_in_time_table: usize,
}

impl Statistics {
    fn num_inferences(&self) -> usize {
        self.inference_types.values().sum()
    }

    fn num_steps(&self) -> usize {
        self.num_deductions + self.num_inferences()
    }

    fn mean_tasks_in_time_table_inference(&self) -> f32 {
        let Some(count) = self
            .inference_types
            .get(&InferenceLabel::Label(Rc::from(TIME_TABLE_LABEL)))
            .copied()
        else {
            return 0.0;
        };

        self.sum_of_tasks_in_time_table as f32 / count as f32
    }
}

fn gather_statistics<Source: BufRead>(mut reader: ProofReader<Source, i32>) -> anyhow::Result<()> {
    let mut statistics = Statistics::default();

    while let Some(step) = reader.next_step()? {
        match step {
            Step::Inference(inference) => {
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

                    statistics.sum_of_tasks_in_time_table += names.len();
                }
            }
            Step::Deduction(_) => {
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

    println!(
        "mean-tasks-in-time-table-inferences: {}",
        statistics.mean_tasks_in_time_table_inference()
    );

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
        Commands::InlineAtomics { output } => inline_atomics(output, reader)?,
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
