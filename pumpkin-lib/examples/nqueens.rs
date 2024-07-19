use pumpkin_lib::constraints;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::results::SatisfactionResult;
use pumpkin_lib::termination::Indefinite;
use pumpkin_lib::variables::TransformableVariable;
use pumpkin_lib::Solver;

fn main() {
    let n = std::env::args()
        .nth(1)
        .expect("Please provide a value for 'n'")
        .parse::<u32>()
        .expect("'n' is not a valid unsigned integer");

    if n < 2 {
        println!("Please provide an 'n > 1'");
        return;
    }

    let mut solver = Solver::default();
    let variables = (0..n)
        .map(|_| solver.new_bounded_integer(0, n as i32 - 1))
        .collect::<Vec<_>>();

    let _ = solver
        .add_constraint(constraints::all_different(variables.clone()))
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
        .post();
    let _ = solver
        .add_constraint(constraints::all_different(diag2))
        .post();

    let mut brancher = solver.default_brancher_over_all_propositional_variables();
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
