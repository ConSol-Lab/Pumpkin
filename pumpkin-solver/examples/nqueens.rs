use std::num::NonZero;
use std::path::PathBuf;

use clap::Parser;
use drcp_format::Format;
use pumpkin_solver::constraints;
use pumpkin_solver::options::SolverOptions;
use pumpkin_solver::proof::ProofLog;
use pumpkin_solver::results::ProblemSolution;
use pumpkin_solver::results::SatisfactionResult;
use pumpkin_solver::termination::Indefinite;
use pumpkin_solver::variables::TransformableVariable;
use pumpkin_solver::Solver;

#[derive(Parser)]
struct Cli {
    /// The size of the chess board.
    n: u32,

    /// The location of the proof.
    ///
    /// If a location is given, the full proof will be logged there.
    #[arg(short, long)]
    proof: Option<PathBuf>,
}

fn main() {
    let Cli {
        n,
        proof: proof_path,
    } = Cli::parse();

    if n < 2 {
        println!("Please provide an 'n > 1'");
        return;
    }

    let Ok(proof_log) = proof_path
        .as_ref()
        .map(|path| ProofLog::cp(path, Format::Text, true, true))
        .transpose()
        .map(|proof| proof.unwrap_or_default())
    else {
        eprintln!(
            "Failed to create proof file at {}",
            proof_path.unwrap().display()
        );
        return;
    };

    let mut solver = Solver::with_options(SolverOptions {
        proof_log,
        ..Default::default()
    });

    let variables = (0..n)
        .map(|i| solver.new_named_bounded_integer(0, n as i32 - 1, format!("q{i}")))
        .collect::<Vec<_>>();

    let _ = solver
        .add_constraint(constraints::all_different(variables.clone()))
        .with_tag(NonZero::new(1).unwrap())
        .post();

    let diag1 = variables
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, var)| var.offset(i as i32))
        .collect::<Vec<_>>();
    let diag2 = variables
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, var)| var.offset(-(i as i32)))
        .collect::<Vec<_>>();

    let _ = solver
        .add_constraint(constraints::all_different(diag1))
        .with_tag(NonZero::new(2).unwrap())
        .post();
    let _ = solver
        .add_constraint(constraints::all_different(diag2))
        .with_tag(NonZero::new(3).unwrap())
        .post();

    let mut brancher = solver.default_brancher();
    match solver.satisfy(&mut brancher, &mut Indefinite) {
        SatisfactionResult::Satisfiable(solution) => {
            let row_separator = format!("{}+", "+---".repeat(n as usize));

            for row in 0..n {
                println!("{row_separator}");

                let queen_col = solution.get_integer_value(variables[row as usize]) as u32;

                for col in 0..n {
                    let string = if queen_col == col { "| * " } else { "|   " };

                    print!("{string}");
                }

                println!("|");
            }

            println!("{row_separator}");
        }
        SatisfactionResult::Unsatisfiable => {
            println!("{n}-queens is unsatisfiable.");
        }
        SatisfactionResult::Unknown => {
            println!("Timeout.");
        }
    }
}
