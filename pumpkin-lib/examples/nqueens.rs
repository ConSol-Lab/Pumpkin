use pumpkin_lib::{
    basic_types::{variables::IntVar, CSPSolverExecutionFlag},
    engine::ConstraintSatisfactionSolver,
    propagators::NotEq,
};

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
        .map(|_| solver.create_new_integer_variable(0, n as i32 - 1))
        .collect::<Vec<_>>();

    all_different(&mut solver, &variables);

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

    all_different(&mut solver, &diag1);
    all_different(&mut solver, &diag2);

    match solver.solve(i64::MAX) {
        CSPSolverExecutionFlag::Feasible => {
            let row_separator = format!("{}+", "+---".repeat(n as usize));

            for row in 0..n {
                println!("{row_separator}");

                let queen_col = solver
                    .get_integer_assignments()
                    .get_assigned_value(variables[row as usize])
                    as u32;

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

fn all_different<Var: IntVar + std::fmt::Debug + 'static>(
    solver: &mut ConstraintSatisfactionSolver,
    variables: &[Var],
) {
    for i in 0..variables.len() {
        for j in i + 1..variables.len() {
            let not_eq = NotEq {
                x: variables[i].clone(),
                y: variables[j].clone(),
            };

            solver.add_propagator(not_eq);
        }
    }
}
