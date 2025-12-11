use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
struct Cli {
    /// Path to the model file (.fzn).
    model_path: PathBuf,

    /// Path to the proof file.
    ///
    /// If the path ends in `.gz`, we assume it is GZipped and the checker will unzip the file
    /// on-the-fly.
    proof_path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    pumpkin_checker::run_checker(cli.model_path, cli.proof_path)
}
