//! Generates the MiniZinc solver configuration files (`minizinc/*.msc`) from the command-line
//! arguments of the solver, such that every option is exposed to MiniZinc.
//!
//! The files can be regenerated with:
//! ```text
//! cargo run -p pumpkin-solver -- --generate-msc minizinc/
//! ```

use std::any::TypeId;
use std::path::Path;
use std::path::PathBuf;

use clap::Arg;
use clap::ArgAction;
use clap::CommandFactory;
use serde_json::Value;
use serde_json::json;

use crate::Args;
use crate::result::PumpkinResult;

/// The solver configurations which are generated; they differ only in their metadata.
const CONFIGURATIONS: [(&str, &str, &str, &str); 2] = [
    (
        "pumpkin.msc",
        "Pumpkin",
        "nl.tudelft.algorithmics.pumpkin",
        "./lib",
    ),
    (
        "pumpkin-for-proofs.msc",
        "PumpkinProof",
        "nl.tudelft.algorithmics.pumpkin_proof",
        "./lib-for-proofs/",
    ),
];

fn generate_msc(name: &str, id: &str, mznlib: &str) -> String {
    let command = Args::command();
    let arguments = command
        .get_arguments()
        .filter(|arg| !arg.is_positional() && !arg.is_hide_set())
        .filter(|arg| !matches!(arg.get_action(), ArgAction::Help | ArgAction::Version));

    // Arguments with a short flag correspond to the standard flags of MiniZinc (e.g. `-a`).
    let (std_flags, extra_flags): (Vec<&Arg>, Vec<&Arg>) =
        arguments.partition(|arg| arg.get_short().is_some());

    let std_flags = std_flags
        .iter()
        .map(|arg| format!("-{}", arg.get_short().unwrap()))
        .collect::<Vec<_>>();
    let extra_flags = extra_flags.into_iter().map(extra_flag).collect::<Vec<_>>();

    let config = json!({
        "name": name,
        "id": id,
        "version": env!("CARGO_PKG_VERSION"),
        "executable": "../target/release/pumpkin-solver",
        "mznlib": mznlib,
        "stdFlags": std_flags,
        "extraFlags": extra_flags,
        "tags": ["cp", "lcg", "int"],
    });

    let mut output = serde_json::to_string_pretty(&config).expect("a JSON value can be serialised");
    output.push('\n');
    output
}

/// Converts an argument to a MiniZinc extra flag of the form `[flag, description, type, default]`.
fn extra_flag(arg: &Arg) -> Value {
    let flag = format!(
        "--{}",
        arg.get_long()
            .expect("non-positional arguments have a long flag")
    );
    let description = arg
        .get_long_help()
        .or(arg.get_help())
        .map(|help| help.to_string())
        .unwrap_or_default();

    let default = arg
        .get_default_values()
        .iter()
        .map(|value| value.to_string_lossy())
        .collect::<Vec<_>>()
        .join(",");

    let (flag_type, default) = if matches!(arg.get_action(), ArgAction::SetTrue) {
        ("bool".to_owned(), "false".to_owned())
    } else if !arg.get_possible_values().is_empty() {
        let values = arg
            .get_possible_values()
            .iter()
            .filter(|value| !value.is_hide_set())
            .map(|value| value.get_name().to_owned())
            .collect::<Vec<_>>();
        (format!("opt:{}", values.join(":")), default)
    } else {
        (value_type(arg), default)
    };

    json!([flag, description, flag_type, default])
}

/// Returns the MiniZinc type corresponding to the type of value which is parsed for `arg`.
fn value_type(arg: &Arg) -> String {
    let type_id = arg.get_value_parser().type_id();

    if type_id == TypeId::of::<u32>() {
        format!("int:0:{}", u32::MAX)
    } else if type_id == TypeId::of::<u64>() || type_id == TypeId::of::<usize>() {
        format!("int:0:{}", u64::MAX)
    } else if type_id == TypeId::of::<i32>() || type_id == TypeId::of::<i64>() {
        "int".to_owned()
    } else if type_id == TypeId::of::<f32>() || type_id == TypeId::of::<f64>() {
        "float".to_owned()
    } else if type_id == TypeId::of::<String>() || type_id == TypeId::of::<PathBuf>() {
        "string".to_owned()
    } else {
        panic!(
            "unsupported type for MiniZinc flag '--{}'",
            arg.get_long().unwrap()
        )
    }
}

/// Writes the MiniZinc solver configurations to `directory`.
pub(crate) fn generate(directory: &Path) -> PumpkinResult<()> {
    for (file, name, id, mznlib) in CONFIGURATIONS {
        std::fs::write(directory.join(file), generate_msc(name, id, mznlib))?;
    }

    Ok(())
}
