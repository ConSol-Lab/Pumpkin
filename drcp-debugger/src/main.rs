use std::fs::File;
use std::io::BufReader;
use std::io::Write;
use std::path::PathBuf;

use anyhow::Context;
use clap::Parser;
use drcp_format::Conclusion;
use drcp_format::Step;
use drcp_format::reader::ProofReader;

#[derive(Parser)]
struct Cli {
    /// The input proof.
    input_proof: PathBuf,
    /// The output proof where literals are replaced with atomics.
    output: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    let input_proof = File::open(&args.input_proof)
        .with_context(|| format!("Failed to open {}", args.input_proof.display()))?;

    let mut reader = ProofReader::<_, i32>::new(BufReader::new(input_proof));
    let mut output = File::create(&args.output)
        .with_context(|| format!("Failed to create {}", args.output.display()))?;

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
