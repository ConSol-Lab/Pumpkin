//! A simple model for disjunctive scheduling using reified constraints
//! Given a set of tasks and their processing times, it finds a schedule such that none of the jobs overlap
//! It thus finds a schedule such that either s_i >= s_j + p_j or s_j >= s_i + p_i (i.e. either job i starts after j or job j starts after i)

use pumpkin_lib::{
    basic_types::{variables::IntVar, Literal},
    engine::ConstraintSatisfactionSolver,
    propagators::LinearLeq,
};

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

    let mut solver = ConstraintSatisfactionSolver::default();

    let start_variables = (0..n_tasks)
        .map(|i| solver.create_new_integer_variable(0, (horizon - processing_times[i]) as i32))
        .collect::<Vec<_>>();

    //Literal which indicates precedence (i.e. if precedence_literals[x][y] => s_y + p_y <= s_x which is equal to s_y - s_x <= -p_y)
    let precedence_literals = (0..n_tasks)
        .map(|_| {
            (0..n_tasks)
                .map(|_| Literal::new(solver.create_new_propositional_variable(), true))
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
            //literal => s_y - s_x <= -p_y)
            linear_less_than_equal_reified(
                &mut solver,
                &variables,
                -(processing_times[y] as i32),
                literal,
            );

            //-literal => -s_y + s_x <= p_y)
            let variables = vec![start_variables[y].scaled(-1), start_variables[x].scaled(1)];
            linear_less_than_equal_reified(
                &mut solver,
                &variables,
                processing_times[y] as i32,
                !literal,
            );

            //Either x starts before y or y start before x
            let _ = solver.add_permanent_clause(vec![literal, precedence_literals[y][x]]);
        }
    }

    if solver.solve(i64::MAX) == pumpkin_lib::basic_types::CSPSolverExecutionFlag::Infeasible {
        panic!("Infeasibility Detected")
    }

    let mut start_variables_and_processing_times = start_variables
        .iter()
        .zip(processing_times)
        .collect::<Vec<_>>();
    start_variables_and_processing_times.sort_by(|(s1, _), (s2, _)| {
        return solver
            .get_integer_assignments()
            .get_assigned_value(**s1)
            .cmp(&solver.get_integer_assignments().get_assigned_value(**s2));
    });

    println!(
        "{}",
        start_variables_and_processing_times
            .iter()
            .map(|(var, processing_time)| format!(
                "[{}, {}]",
                solver.get_integer_assignments().get_assigned_value(**var),
                solver.get_integer_assignments().get_assigned_value(**var)
                    + *processing_time as i32
            ))
            .collect::<Vec<_>>()
            .join(" - ")
    );
}

fn linear_less_than_equal_reified<Var: IntVar + 'static>(
    solver: &mut ConstraintSatisfactionSolver,
    vars: &[Var],
    c: i32,
    reif_literal: Literal,
) {
    if solver.add_propagator(LinearLeq::reified(vars.into(), c, reif_literal)) {
        panic!("Adding propagator led to conflict");
    }
}
