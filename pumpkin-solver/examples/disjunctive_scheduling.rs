//! A simple model for disjunctive scheduling using reified constraints
//! Given a set of tasks and their processing times, it finds a schedule such that none of the jobs
//! overlap. The optimal schedule is thus all tasks scheduled right after each other.
//!
//! For two tasks x and y, either x ends before y starts, or y ends before x starts. So if s_i is
//! the start time of task i and p_i is then we can express the condition that x ends before y
//! starts as s_x + p_x <= s_y, and that y ends before x starts as s_y + p_y <= s_x.
//!
//! To ensure that one of these occurs, we create two Boolean variables, l_xy and l_yx, to signify
//! the two possibilities, and then post the constraint (l_xy \/ l_yx).

use pumpkin_core::constraints;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::results::ProblemSolution;
use pumpkin_core::results::SatisfactionResult;
use pumpkin_core::termination::Indefinite;
use pumpkin_core::variables::TransformableVariable;
use pumpkin_core::Solver;

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

    // Creates a dummy constraint tag; since this example does not support proof logging the
    // constraint tag does not matter.
    let constraint_tag = solver.new_constraint_tag();

    let start_variables = (0..n_tasks)
        .map(|i| solver.new_bounded_integer(0, (horizon - processing_times[i]) as i32))
        .collect::<Vec<_>>();

    // Literal which indicates precedence (i.e. precedence_literals[x][y] <=> x ends before y
    // starts)
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
            // precedence_literals[x][y] <=> x ends before y starts
            let literal = precedence_literals[x][y];
            // literal <=> (s_x + p_x <= s_y)
            // equivelent to literal <=> (s_x - s_y <= -p_x)
            // So the variables are -s_y and s_x, and the rhs is -p_x
            let variables = vec![start_variables[y].scaled(-1), start_variables[x].scaled(1)];
            let _ = constraints::less_than_or_equals(
                variables,
                -(processing_times[x] as i32),
                constraint_tag,
            )
            .reify(&mut solver, literal);

            // Either x starts before y or y start before x
            let _ = solver.add_clause(
                [
                    literal.get_true_predicate(),
                    precedence_literals[y][x].get_true_predicate(),
                ],
                constraint_tag,
            );
        }
    }

    let mut brancher = solver.default_brancher();
    if matches!(
        solver.satisfy(&mut brancher, &mut Indefinite),
        SatisfactionResult::Unsatisfiable(_),
    ) {
        panic!("Infeasibility Detected")
    }
    match solver.satisfy(&mut brancher, &mut Indefinite) {
        SatisfactionResult::Satisfiable(satisfiable) => {
            let solution = satisfiable.solution();

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
        SatisfactionResult::Unsatisfiable(_) => panic!("Infeasibility Detected"),
        SatisfactionResult::Unknown(_) => println!("Timeout."),
    };
}
