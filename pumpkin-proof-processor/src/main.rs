//! The proof processor turns a solver's scaffold, its learned nogoods, into a full proof. Its
//! input is the FlatZinc model and the scaffold. A croc scaffold starts with a preamble stating
//! the solver's preprocessing: its views, constants, literal predicates, decompositions and
//! symmetry and dominance breaking steps, followed by a marker line and the nogoods. The processor
//! reads the preamble, applies exactly that preprocessing, builds its solver once on the solver's
//! variables when it reaches the marker, reads the nogoods as they arrive, derives each by its own
//! propagation, and writes the proof headed by the preamble's steps. The scaffold is read once,
//! front to back, so it may be a pipe; `-` reads it from the standard input.

mod deduction_database;
mod deduction_propagator;
mod deduction_rule;
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

use anyhow::Context;
use clap::Parser;
use clap_verbosity_flag::InfoLevel;
use clap_verbosity_flag::Verbosity;
use drcp_format::reader::ProofReader;
use drcp_format::writer::ProofWriter;
use pumpkin_core::proof::ProofLog;
use pumpkin_core::state::State;
use pumpkin_propagators::cumulative::options::CumulativeOptions;
use pumpkin_propagators::cumulative::options::CumulativePropagationMethod;
use pumpkin_propagators::cumulative::time_table::CumulativeExplanationType;
use pumpkin_solver::Solver;
use pumpkin_solver::core::options::SolverOptions;
use pumpkin_solver::flatzinc::CompilationOptions;
use pumpkin_solver::flatzinc::ProofFormat as CompilerProofFormat;
use pumpkin_solver::flatzinc::ProofType;
use pumpkin_solver::flatzinc::parse_and_compile;
use pumpkin_solver::flatzinc::preamble::Preamble;

use crate::processor::DeductionStoreKind;
use crate::processor::ProofOutput;
use crate::processor::ProofProcessor;
use crate::variables::Variables;

#[derive(Parser)]
struct Cli {
    #[command(flatten)]
    verbose: Verbosity<InfoLevel>,

    /// Path to the model file (.fzn).
    model_path: PathBuf,

    /// Path to the scaffold file, which may be a pipe; `-` for the standard input.
    scaffold_path: PathBuf,

    /// Path to the full proof (output).
    full_proof_path: PathBuf,

    /// The format to write the proof in. The model is compiled as the solver compiles it for a
    /// proof in that format, so the scaffold has to come from the solver run with the matching
    /// `--proof-format`: `drcp` for `drcp`, `croc` for `blocks`. A croc scaffold starts with a
    /// preamble that states the solver's preprocessing.
    #[arg(long, value_enum, default_value_t = ProofFormat::Drcp)]
    format: ProofFormat,

    /// How a proof in the block format derives each nogood: `resolution` writes a resolution
    /// chain, `refutation` the nogood and the inferences that refute it in the order they
    /// propagate.
    #[arg(long, value_enum, default_value_t = NogoodDerivation::Resolution)]
    nogood_derivation: NogoodDerivation,

    /// How a proof in the block format is written: in the binary form the checker recognises in
    /// a file, or as text.
    #[arg(long, value_enum, default_value_t = ProofEncoding::Binary)]
    proof_encoding: ProofEncoding,

    /// Write a proof in the block format for the lenient reading of the checker, within the root
    /// domains: the declared domains and the nogoods over one variable the proof holds
    /// (experimental). The proof starts with the header `m lenient` and leaves out the facts and
    /// inferences whose atomic constraint these entail, and the premises of an inference they
    /// entail; the unit nogoods of the facts at the root are still derived once.
    #[arg(long)]
    root_domains: bool,

    /// With `--root-domains` and `--nogood-derivation refutation`, write each inference by the
    /// rules `nogood`, `linear_bounds` and `element` (also reified and half-reified) as its
    /// consequent alone, `> <consequent>`, for the lenient reading to take its premises from the
    /// current domains of the refutation (experimental).
    #[arg(long)]
    omit_premises: bool,

    /// Leave out the symmetry and dominance breaking steps of the head whose constraint no kept
    /// inference cites. This holds the head back until trimming is done, which a processor that
    /// writes its proof as it derives it cannot do.
    #[arg(long)]
    drop_unused_breaking_steps: bool,

    /// How the scaffold's deductions are held while trimming.
    #[arg(long, value_enum, default_value_t = DeductionStore::DatabaseMerged)]
    deduction_store: DeductionStore,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
enum DeductionStore {
    /// Two propagators per deduction, registered on every predicate of its nogood.
    PerDeduction,
    /// Three shared databases with two watched predicates per nogood, one per priority level.
    Database,
    /// Two shared databases: unmarked conflict detection and propagation at one priority.
    DatabaseMerged,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
enum ProofFormat {
    /// The DRCP format, where inferences are numbered and a deduction refers to them.
    Drcp,
    /// The croc block format of fzn-croc-check.
    Blocks,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
enum NogoodDerivation {
    /// A resolution chain from the conflict.
    Resolution,
    /// A refutation of the nogood by the inferences in the order they propagate.
    Refutation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
enum ProofEncoding {
    /// The binary form.
    Binary,
    /// The text form.
    Text,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    if cli.root_domains && cli.format != ProofFormat::Blocks {
        anyhow::bail!("--root-domains needs --format blocks");
    }
    if cli.omit_premises
        && (!cli.root_domains || cli.nogood_derivation != NogoodDerivation::Refutation)
    {
        anyhow::bail!("--omit-premises needs --root-domains and --nogood-derivation refutation");
    }

    env_logger::Builder::new()
        .format_timestamp(None)
        .format_target(false)
        .filter_level(cli.verbose.log_level_filter())
        .target(env_logger::Target::Stdout)
        .init();

    // A proof in the block format is written through the proof log the compiler fills: the
    // steps that state the views, the rules of the constraints and their reifications.
    let mut proof_log = match cli.format {
        ProofFormat::Drcp => ProofLog::default(),
        ProofFormat::Blocks => ProofLog::blocks(
            &cli.full_proof_path,
            cli.nogood_derivation == NogoodDerivation::Resolution,
            cli.proof_encoding == ProofEncoding::Binary,
            cli.root_domains,
        )
        .with_context(|| "Failed to create proof writer")?,
    };
    if cli.omit_premises {
        proof_log.write_premise_free_inferences();
    }

    // The processor reads the solver's preprocessing from the head of the scaffold and builds its
    // solver once, on the variables the nogoods are written over, before reading any nogood.
    let mut scaffold =
        open_scaffold(&cli.scaffold_path).with_context(|| "Failed to read the scaffold")?;
    let preamble = read_preamble(&cli, &mut scaffold)?;
    let (state, proof_log) =
        build_solver(&cli, preamble, proof_log).with_context(|| "Failed to compile the model")?;

    let output = match cli.format {
        ProofFormat::Drcp => {
            let sink = create_sink(&cli.full_proof_path)
                .with_context(|| "Failed to create proof writer")?;
            ProofOutput::Drcp(ProofWriter::new(sink))
        }
        ProofFormat::Blocks => ProofOutput::Blocks(proof_log),
    };

    let Some(state) = state else {
        anyhow::bail!("posting the model's constraints made it infeasible at the root");
    };

    let variables = Variables::from_state(&state);
    let mut proof_processor = ProofProcessor::new(state, variables);

    let store = match cli.deduction_store {
        DeductionStore::PerDeduction => DeductionStoreKind::PerDeduction,
        DeductionStore::Database => DeductionStoreKind::Database,
        DeductionStore::DatabaseMerged => DeductionStoreKind::DatabaseMerged,
    };
    proof_processor.set_deduction_store(store);
    proof_processor.set_drop_unused_steps(cli.drop_unused_breaking_steps);

    proof_processor.process(ProofReader::new(scaffold), output)?;

    Ok(())
}

/// The preamble at the head of a croc scaffold, read up to the marker line that ends it, which
/// leaves `scaffold` at the first nogood. A scaffold for a proof in the DRCP format has none.
fn read_preamble(cli: &Cli, scaffold: &mut impl BufRead) -> anyhow::Result<Preamble> {
    match cli.format {
        ProofFormat::Drcp => Ok(Preamble::default()),
        ProofFormat::Blocks => Preamble::read_head_of_scaffold(scaffold)
            .with_context(|| "Failed to read the preamble of the scaffold"),
    }
}

/// Build the solver once with the solver's FlatZinc compiler, which posts the propagators,
/// constraint tags, rule names and decompositions the solver does for every constraint, with
/// the preprocessing applied rather than decided.
fn build_solver(
    cli: &Cli,
    preprocessing: Preamble,
    proof_log: ProofLog,
) -> anyhow::Result<(Option<State>, ProofLog)> {
    let mut solver = Solver::with_options(SolverOptions {
        proof_log,
        ..Default::default()
    });

    let (proof_format, proof_type) = match cli.format {
        ProofFormat::Drcp => (CompilerProofFormat::Drcp, ProofType::Scaffold),
        ProofFormat::Blocks => (CompilerProofFormat::Croc, ProofType::Blocks),
    };

    let model = File::open(&cli.model_path)?;
    let _ = parse_and_compile(
        &mut solver,
        model,
        CompilationOptions {
            // The solver's defaults for its cumulative options.
            cumulative_options: CumulativeOptions::new(
                false,
                CumulativeExplanationType::default(),
                true,
                CumulativePropagationMethod::default(),
                false,
            ),
            proof_type: Some(proof_type),
            proof_format,
            preamble: Some(preprocessing),
            ..Default::default()
        },
    )?;

    Ok(solver.into_state_and_proof_log())
}

/// The scaffold, decompressed when its name ends in `.gz`; `-` is the standard input.
fn open_scaffold(path: &Path) -> anyhow::Result<Box<dyn BufRead>> {
    if path == Path::new("-") {
        return Ok(Box::new(BufReader::new(std::io::stdin())));
    }

    let file = File::open(path)?;

    if path.extension().is_some_and(|ext| ext == "gz") {
        Ok(Box::new(BufReader::new(flate2::read::GzDecoder::new(file))))
    } else {
        Ok(Box::new(BufReader::new(file)))
    }
}

fn create_sink(path: impl AsRef<Path>) -> anyhow::Result<Box<dyn Write>> {
    let file = File::create(path.as_ref())?;

    if path.as_ref().extension().is_some_and(|ext| ext == "gz") {
        let encoder = flate2::write::GzEncoder::new(file, flate2::Compression::fast());
        Ok(Box::new(BufWriter::new(encoder)))
    } else {
        Ok(Box::new(BufWriter::new(file)))
    }
}
