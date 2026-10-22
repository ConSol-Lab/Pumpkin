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

/// The standard flags of MiniZinc which have a short form (see
/// <https://docs.minizinc.dev/en/stable/fzn-spec.html#solver-configuration-files>).
const MINIZINC_STD_FLAGS: [char; 9] = ['a', 'f', 'i', 'n', 'p', 'r', 's', 't', 'v'];

fn generate_msc(name: &str, id: &str, mznlib: &str) -> String {
    let command = Args::command();
    let arguments = command
        .get_arguments()
        .filter(|arg| !arg.is_positional() && !arg.is_hide_set())
        .filter(|arg| !matches!(arg.get_action(), ArgAction::Help | ArgAction::Version));

    // Arguments whose short flag is a standard flag of MiniZinc (e.g. `-a`) are exposed as such;
    // all other arguments are exposed as extra flags.
    let (std_flags, extra_flags): (Vec<&Arg>, Vec<&Arg>) = arguments.partition(|arg| {
        arg.get_short()
            .is_some_and(|short| MINIZINC_STD_FLAGS.contains(&short))
    });

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
    let flag = match (arg.get_long(), arg.get_short()) {
        (Some(long), _) => format!("--{long}"),
        (None, Some(short)) => format!("-{short}"),
        (None, None) => unreachable!("non-positional arguments have a long or short flag"),
    };
    let summary = arg
        .get_long_help()
        .or(arg.get_help())
        .map(|help| summary(&help.to_string()))
        .unwrap_or_default();

    let default = arg
        .get_default_values()
        .iter()
        .map(|value| value.to_string_lossy())
        .collect::<Vec<_>>()
        .join(",");

    let (flag_type, default, description) = if matches!(arg.get_action(), ArgAction::SetTrue) {
        ("bool".to_owned(), "false".to_owned(), summary)
    } else if !arg.get_possible_values().is_empty() {
        let values = arg
            .get_possible_values()
            .iter()
            .filter(|value| !value.is_hide_set())
            .map(|value| value.get_name().to_owned())
            .collect::<Vec<_>>();
        let description = format!(
            "{summary} [default: {default}] [possible values: {}]",
            values.join(", ")
        );
        (format!("opt:{}", values.join(":")), default, description)
    } else {
        let flag_type = value_type(arg);
        // Strip the bounds from the type, e.g. `int:0:4294967295` is shown as `int`.
        let type_name = flag_type
            .split(':')
            .next()
            .expect("split yields at least one item");
        let description = if default.is_empty() {
            format!("{summary} [possible values: {type_name}]")
        } else {
            format!("{summary} [default: {default}] [possible values: {type_name}]")
        };
        (flag_type, default, description)
    };

    json!([flag, description, flag_type, default])
}

/// Condenses the help of an argument into a single line, such that it is displayed nicely by
/// `minizinc --help`, which does not indent the continuation lines of a description.
///
/// The summary is the first paragraph of the help, up to the start of any list.
fn summary(help: &str) -> String {
    help.lines()
        .map(str::trim)
        .take_while(|line| !line.is_empty() && !line.starts_with("- "))
        .collect::<Vec<_>>()
        .join(" ")
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
        panic!("unsupported type for MiniZinc flag '{}'", arg.get_id())
    }
}

/// Writes the MiniZinc solver configurations to `directory`.
pub(crate) fn generate(directory: &Path) -> PumpkinResult<()> {
    for (file, name, id, mznlib) in CONFIGURATIONS {
        std::fs::write(directory.join(file), generate_msc(name, id, mznlib))?;
    }

    Ok(())
}
