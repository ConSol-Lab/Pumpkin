use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use anyhow::Context;
use clap::Parser;
use drcp_format::reader::ProofReader;
use drcp_format::steps::Conclusion;
use drcp_format::steps::Step;
use drcp_format::LiteralDefinitions;

#[derive(Parser)]
struct Cli {
    /// The input proof.
    input_proof: PathBuf,
    /// The input literals.
    input_lits: PathBuf,
    /// The output proof where literals are replaced with atomics.
    output: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    let input_proof = File::open(&args.input_proof)
        .with_context(|| format!("Failed to open {}", args.input_proof.display()))?;

    let input_lits = File::open(&args.input_lits)
        .with_context(|| format!("Failed to open {}", args.input_proof.display()))?;

    let literals = LiteralDefinitions::<String>::parse(input_lits).with_context(|| {
        format!(
            "Failed to parse literal definitions from {}.",
            args.input_lits.display()
        )
    })?;

    let mut reader = ProofReader::new(input_proof, literals);
    let mut output = File::create(&args.output)
        .with_context(|| format!("Failed to create {}", args.output.display()))?;

    while let Some(step) = reader.next_step()? {
        match step {
            Step::Inference(inference) => {
                write!(output, "i {}", inference.id)?;

                for premise in inference.premises {
                    write!(output, " {}", premise)?;
                }

                if let Some(propagated) = inference.propagated {
                    write!(output, " 0 {}", propagated)?;
                }

                if let Some(label) = inference.hint_label {
                    write!(output, " l:{label}")?;
                }

                if let Some(constraint_id) = inference.hint_constraint_id {
                    write!(output, " c:{constraint_id}")?;
                }

                writeln!(output)?;
            }
            Step::Nogood(nogood) => {
                write!(output, "n {}", nogood.id)?;

                for literal in nogood.literals {
                    write!(output, " {}", literal)?;
                }

                write!(output, " 0")?;

                for hint in nogood.hints.iter().flatten() {
                    write!(output, " {}", hint)?;
                }

                writeln!(output)?;
            }
            Step::Delete(step) => writeln!(output, "d {}", step.id)?,
            Step::Conclusion(conclusion) => match conclusion {
                Conclusion::Unsatisfiable => writeln!(output, "c UNSAT")?,
                Conclusion::Optimal(bound) => writeln!(output, "c {bound}")?,
            },
        }
    }

    Ok(())
}
