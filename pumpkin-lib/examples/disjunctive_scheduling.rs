//! A simple model for disjunctive scheduling using reified constraints
//! Given a set of tasks and their processing times, it finds a schedule such that none of the jobs
//! overlap It thus finds a schedule such that either s_i >= s_j + p_j or s_j >= s_i + p_i (i.e.
//! either job i starts after j or job j starts after i)

use pumpkin_lib::constraints;
use pumpkin_lib::constraints::Constraint;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::results::SatisfactionResult;
use pumpkin_lib::termination::Indefinite;
use pumpkin_lib::variables::TransformableVariable;
use pumpkin_lib::Solver;

fn main() {
    let mut args = std::env::args();

    let n_tasks = args
        .nth(1)
        .expect("Please provide a number of tasks")
        .parse::<usize>()
        .expect("Not a valid usized");
    let processing_times = args
        .take(n_tasks)
        .map(|arg| arg.parse::<usize>())
        .collect::<Result<Vec<_>, _>>()
        .expect("The provided processing times are not valid unsigned integers");
    assert_eq!(
        processing_times.len(),
        n_tasks,
        "Provided fewer than `n_tasks` processing times."
    );

    let horizon = processing_times.iter().sum::<usize>();

    let mut solver = Solver::default();

    let start_variables = (0..n_tasks)
        .map(|i| solver.new_bounded_integer(0, (horizon - processing_times[i]) as i32))
        .collect::<Vec<_>>();

    // Literal which indicates precedence (i.e. if precedence_literals[x][y] => s_y + p_y <= s_x
    // which is equal to s_y - s_x <= -p_y)
    let precedence_literals = (0..n_tasks)
        .map(|_| {
            (0..n_tasks)
                .map(|_| solver.new_literal())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    for x in 0..n_tasks {
        for y in 0..n_tasks {
            if x == y {
                continue;
            }
            let literal = precedence_literals[x][y];
            let variables = vec![start_variables[y].scaled(1), start_variables[x].scaled(-1)];
            // literal => s_y - s_x <= -p_y)
            let _ =
                constraints::less_than_or_equals(variables.clone(), -(processing_times[y] as i32))
                    .implied_by(&mut solver, literal, None);

            //-literal => -s_y + s_x <= p_y)
            let variables = vec![start_variables[y].scaled(-1), start_variables[x].scaled(1)];
            let _ = constraints::less_than_or_equals(variables.clone(), processing_times[y] as i32)
                .implied_by(&mut solver, literal, None);

            // Either x starts before y or y start before x
            let _ = solver.add_clause([literal, precedence_literals[y][x]]);
        }
    }

    let mut brancher = solver.default_brancher_over_all_propositional_variables();
    if matches!(
        solver.satisfy(&mut brancher, &mut Indefinite),
        SatisfactionResult::Unsatisfiable,
    ) {
        panic!("Infeasibility Detected")
    }
    match solver.satisfy(&mut brancher, &mut Indefinite) {
        SatisfactionResult::Satisfiable(solution) => {
            let mut start_variables_and_processing_times = start_variables
                .iter()
                .zip(processing_times)
                .collect::<Vec<_>>();
            start_variables_and_processing_times.sort_by(|(s1, _), (s2, _)| {
                solution
                    .get_integer_value(**s1)
                    .cmp(&solution.get_integer_value(**s2))
            });

            println!(
                "{}",
                start_variables_and_processing_times
                    .iter()
                    .map(|(var, processing_time)| format!(
                        "[{}, {}]",
                        solution.get_integer_value(**var),
                        solution.get_integer_value(**var) + *processing_time as i32
                    ))
                    .collect::<Vec<_>>()
                    .join(" - ")
            );
        }
        SatisfactionResult::Unsatisfiable => panic!("Infeasibility Detected"),
        SatisfactionResult::Unknown => println!("Timeout."),
    }
}
