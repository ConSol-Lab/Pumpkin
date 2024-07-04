use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_lib::constraints::ConstraintsExt;
use pumpkin_lib::engine::termination::indefinite::Indefinite;
use pumpkin_lib::engine::variables::TransformableVariable;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

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

    let mut solver = ConstraintSatisfactionSolver::default();
    let variables = (0..n)
        .map(|_| solver.create_new_integer_variable(0, n as i32 - 1, None))
        .collect::<Vec<_>>();

    let _ = solver.all_different(variables.clone());

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

    let _ = solver.all_different(diag1);
    let _ = solver.all_different(diag2);

    let mut brancher =
        IndependentVariableValueBrancher::default_over_all_propositional_variables(&solver);
    match solver.solve(&mut Indefinite, &mut brancher) {
        CSPSolverExecutionFlag::Feasible => {
            let row_separator = format!("{}+", "+---".repeat(n as usize));

            for row in 0..n {
                println!("{row_separator}");

                let queen_col = solver
                    .get_assigned_integer_value(&variables[row as usize])
                    .unwrap() as u32;

                for col in 0..n {
                    let string = if queen_col == col { "| * " } else { "|   " };

                    print!("{string}");
                }

                println!("|");
            }

            println!("{row_separator}");
        }

        CSPSolverExecutionFlag::Infeasible => {
            println!("{n}-queens is unsatisfiable.");
        }

        CSPSolverExecutionFlag::Timeout => {
            println!("Timeout.");
        }
    }
}
